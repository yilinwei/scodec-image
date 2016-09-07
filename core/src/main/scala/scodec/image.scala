package scodec
package codecs

import java.io.{File, FileInputStream}

import scala.annotation

import scodec.bits._

object image {

}

//http://www.libpng.org/pub/png/spec/1.2/png-1.2.pdf
private[scodec] object PNG {

  //PNG.codec(PNG.ColourType.Greyscale)

  val DOS: Char = 0x1a.toChar
  val Unix: Char = 0x0a.toChar

  object header extends Codec[Char] {

    val png = constant(hex"89504e47")
    val dos = constant(hex"0d0a1a0a")
    val unix = constant(hex"0000000a")

    val unrecognized = fail[Unit](Err("Unrecognized platform"), Err("Unrecognized platform"))

    val sizeBound = SizeBound.exact(64)

    def decode(bitVector: BitVector): Attempt[DecodeResult[Char]] = {
      png.flatMap { _ =>
        Decoder.choiceDecoder[Char](
          dos.map(_ =>  DOS),
          unix.map(_ =>  Unix)
        )
      }.decode(bitVector) match {
        case Attempt.Successful(DecodeResult(eof, remainder)) =>
          Attempt.successful(DecodeResult(eof, remainder))
        case Attempt.Failure(err) =>
          Attempt.failure(Err("PNG header does not match expected format"))
      }
    }

    def encode(char: Char): Attempt[BitVector] = {
      (png ~> (char match {
        case DOS => dos
        case Unix => unix
        case _ => unrecognized
      })).encode(())
    }
  }

  sealed trait ColourType {
    def components: Int = this match {
      case ColourType.Greyscale => 1
      case ColourType.Truecolour => 3
      case ColourType.IndexedColour => 1
      case ColourType.GreyscaleAlpha => 2
      case ColourType.TruecolourAlpha => 4  
    }
  }

  object ColourType {
    case object Greyscale extends ColourType
    case object Truecolour extends ColourType
    case object IndexedColour extends ColourType
    case object GreyscaleAlpha extends ColourType
    case object TruecolourAlpha extends ColourType

    def from(i: Int): Option[ColourType] = i match {
      case 0 => Some(Greyscale)
      case 2 => Some(Truecolour)
      case 3 => Some(IndexedColour)
      case 4 => Some(GreyscaleAlpha)
      case 6 => Some(TruecolourAlpha)
      case _ => None  
    }

    def validateBitDepth(colour: ColourType, depth: Int): Boolean = (depth, colour) match {
      case (1, Greyscale) => true
      case (2, Greyscale) => true
      case (4, Greyscale) => true
      case (8, Greyscale) => true
      case (16, Greyscale) => true
      case (8, Truecolour) => true
      case (16, Truecolour) => true
      case (1, IndexedColour) => true
      case (2, IndexedColour) => true
      case (4, IndexedColour) => true
      case (8, IndexedColour) => true
      case (8, GreyscaleAlpha) => true
      case (16, GreyscaleAlpha) => true
      case _ => false  
    }
  }

  object BitDepth {

    val allowed: Map[ColourType, Set[Int]] = Map(
      ColourType.Greyscale -> Set(1, 2, 4, 8, 16),
      ColourType.Truecolour -> Set(8, 16),
      ColourType.IndexedColour -> Set(1, 2, 4, 8),
      ColourType.GreyscaleAlpha -> Set(8, 16)
    )

    def from(colour: ColourType, depth: Int): Option[Int] = allowed.get(colour).flatMap { depths =>
      if(depths.contains(depth)) Some(depth) else None
    }
  }


  sealed trait FilterMethod

  object FilterMethod {

    case object Zero extends FilterMethod

    final class ZeroDecoder(bpp: Int, components: Int, width: Long, height: Long) extends Decoder[ByteVector] {

      val lineWith = width * (bpp * components)

      def decode(bitVector: BitVector): Attempt[DecodeResult[ByteVector]] = {
        //Since is likely to be large, hand roll to get tail recursion
        @annotation.tailrec
        def go(p: ByteVector, r: ByteVector, h: Long, acc: Either[Attempt.Failure, ByteVector]): Either[Attempt.Failure, ByteVector] = {
          if(h == 0) acc else
            acc match {
              case Right(aa) =>
                uint8.decode(r.toBitVector) match {
                  case Attempt.Successful(DecodeResult(ft, rem)) =>
                    val (c, rr) = rem.splitAt(lineWith)
                    val proc = ft match {
                      case 0 => identity[ByteVector] _
                      case 1 => FilterType.sub(bpp) _
                      case 2 => FilterType.up(p) _
                      case 3 => FilterType.average(bpp, p) _
                      case 4 => FilterType.paeth(bpp, p) _ 
                    }
                    val pp =  proc(c.toByteVector)
                    go(pp, rr.toByteVector, h - 1, Right(aa ++ pp))
                  case fail @ Attempt.Failure(_) => Left(fail)
                }
              case term @ Left(_) => term
            }
        }
        go(ByteVector.fill(lineWith)(0), bitVector.toByteVector, height, Right(ByteVector.empty)).right.map { res =>
          Attempt.successful(DecodeResult(res, bitVector.drop(width * height * FilterType.nearest(bpp))))
        }.merge  
      }
    }

    def from(i: Int): Option[FilterMethod] = i match {
      case 0 => Some(Zero)
      case _ => None
    }
  }




  object FilterType {

    private def uminus(x: Byte, y: Byte): Int = (x & 0xff) - (y & 0xff)
    private def uplus(x: Byte, y: Byte): Int = (x & 0xff) + (y & 0xff)

    def minus(x: ByteVector, y: ByteVector): ByteVector = 
      x.zipWithI(y)(uminus(_, _) % 256)

    def nearest(bpp: Int): Int = Math.ceil(bpp.toFloat / 8f).toInt

    def left(bpp: Int, scan: ByteVector): ByteVector = 
      ByteVector.fill(nearest(bpp))(0) ++ scan

    def sub(bpp: Int)(scan: ByteVector): ByteVector =
      minus(scan, left(bpp, scan))

    def up(prev: ByteVector)(scan: ByteVector): ByteVector = minus(prev, scan)

    def average(bpp: Int, prev: ByteVector)(scan: ByteVector): ByteVector =
      scan.zipWithI2(left(bpp, scan), prev) { (s, l, p) =>
        uminus(s, Math.floor(uplus(l, p) / 2).toByte) % 256
      }
  
    def paeth(bpp: Int, prev: ByteVector)(scan: ByteVector): ByteVector = {
      scan.zipWithI3(left(bpp, scan), prev, left(bpp, prev)) { (s, a, b, c) =>
        val p = uplus(a, b) - (c & 0xff)
        val pa = Math.abs(p - (a & 0xff))
        val pb = Math.abs(p - (b & 0xff))
        val pc = Math.abs(p - (c & 0xff))
        val pp = (if(pa <= pb && pa <= pc) a else
          if(pb <= pc) b else c)
        uminus(s, pp) % 256
      }
    }

  }

  sealed trait Interlacing

  object Interlacing {
    case object None extends Interlacing
    case object Adam7 extends Interlacing

    def from(i: Int): Option[Interlacing] = i match {
      case 0 => Some(None)
      case 1 => Some(Adam7)
      case _ => Option.empty  
    }
  }

  object frame extends Codec[(BitVector, BitVector)] {

    val sizeBound = SizeBound.unknown

    def decode(bitVector: BitVector): Attempt[DecodeResult[(BitVector, BitVector)]] = {
      uint32.decode(bitVector) match {
        case Attempt.Successful(DecodeResult(length, remainder)) =>
          val (data, n) = remainder.splitAt((4 + length) * 8)
          val (crc32, nn) = n.splitAt(32)
          Attempt.successful(DecodeResult(data -> crc32, nn))
        case err @ Attempt.Failure(_) => err
      }
    }

    def encode(data: (BitVector, BitVector)): Attempt[BitVector] = ???
  }

  case class IHDR(
    width: Long,
    height: Long,
    colourType: ColourType,
    bitDepth: Int,
    filterMethod: FilterMethod,
    interlace: Interlacing
  )

  def cc4(cc4: String): Codec[Unit] = new Codec[Unit] {
    val sizeBound = SizeBound.exact(32L)

    def decode(bitVector: BitVector): Attempt[DecodeResult[Unit]] =
      ascii.decode(bitVector.take(32L)) match {
        case att @ Attempt.Successful(DecodeResult(found, remainder)) =>
          if(found == cc4) Attempt.successful(DecodeResult((), bitVector.drop(32L))) else
            Attempt.Failure(Err(s"expected chunk type: $cc4, obtained chunk type: $found"))
        case _ => Attempt.Failure(Err(s"unable to find chunk type"))
      }
    def encode(u: Unit): Attempt[BitVector] = ???
  }

  val ihdr: Codec[IHDR] = checksummed(new Codec[IHDR] {

    val sizeBound = SizeBound.exact(68 + 32)

    def decode(bitVector: BitVector): Attempt[DecodeResult[IHDR]] = {
      (cc4("IHDR") ~>
          uint32 ~
          uint32 ~
          uint8 ~
          uint8 ~
          uint8 ~
          uint8 ~
          uint8).decode(bitVector) match {
        case Attempt.Successful(DecodeResult(((((((width, height), bd), ct), _), fm), ic), remainder)) =>
          val valid = for {
            colour <- ColourType.from(ct)
            filterMethod <- FilterMethod.from(fm)
            bitDepth <- BitDepth.from(colour, bd)
            interlace <- Interlacing.from(ic)
          } yield Attempt.successful(
            DecodeResult(
              IHDR(
                width,
                height,
                colour,
                bitDepth,
                filterMethod,
                interlace), remainder))

          valid.getOrElse(Attempt.failure(Err("unrecognized IHDR colour type format")))  
        case Attempt.Failure(err) => Attempt.Failure(Err(s"unable to read chunk IHDR due to: $err"))  
      }
    }    
    def encode(ihdr: IHDR): Attempt[BitVector] = ???
  }, crc.crc32, frame)

  //TODO: support multiple IDAT chunks
  def idat(ihdr: IHDR): Decoder[ByteVector] = new Decoder[ByteVector] {
    def decode(bitVector: BitVector): Attempt[DecodeResult[ByteVector]] = {
      checksummed(cc4("IDAT") ~> zlib(bits), crc.crc32, frame).decode(bitVector).flatMap {
        case DecodeResult(unzipped, rem) =>
          (new FilterMethod.ZeroDecoder(ihdr.bitDepth, ihdr.colourType.components, ihdr.width, ihdr.height)).decode(unzipped)
      }
    }

  }


  val unknown: Codec[Unit] = checksummed(new Codec[Unit] {

    val sizeBound = SizeBound.unknown

    def decode(bitVector: BitVector): Attempt[DecodeResult[Unit]] = {
      ascii.decode(bitVector.take(32L)) match {
        case att @ Attempt.Successful(DecodeResult(cc4, _)) =>
          if(cc4(0).isUpper)
            Attempt.Failure(Err(s"unable to decode critical chunk $cc4"))
          else
            Attempt.successful(DecodeResult((), BitVector.empty))
        case fail @ Attempt.Failure(err) => Attempt.failure(Err(s"not found chunk type info: $err"))
      }
    }
    def encode(unit: Unit): Attempt[BitVector] = Attempt.successful(BitVector.empty)
   }, crc.crc32, frame)

  val png = (PNG.header ~ PNG.ihdr <~ PNG.unknown <~ PNG.unknown <~ PNG.unknown).flatMap {
    case (eof, ihdr) =>
      idat(ihdr) 
  } 

}


object PNGTest {
  def main(args: Array[String]): Unit = {
    val f = new FileInputStream(new File("E:\\\\Yilin\\\\Downloads\\\\typelevel.png"))
    val bv = BitVector.fromInputStream(f)
    val m1 = System.currentTimeMillis()
    //PNG.png.decode(bv)
    val ii = javax.imageio.ImageIO.read(f)
    (ii.getRaster().getDataBuffer()).asInstanceOf[java.awt.image.DataBufferByte].getData()
    val m2 = System.currentTimeMillis()
    println(m2 - m1)
  }
}
