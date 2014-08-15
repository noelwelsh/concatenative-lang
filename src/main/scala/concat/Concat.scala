package concat

import shapeless._
import shapeless.nat._
import shapeless.ops.hlist._

case class Concat[L <: HList](stack: L = HNil) {
  import Evidence._

  // Stack manipulation

  def push[A](a: A): Concat[A :: L] =
    Concat(a :: stack)

  def dup(implicit ev: IsHCons[L]): Concat[ev.H :: L] = {
    val hd = stack.head
    Concat(hd +: stack)
  }

  def drop(implicit ev: IsHCons[L]): Concat[ev.T] =
    Concat(ev.tail(stack))

  def swap[A, B](implicit ev: IsBinary[A, B, L]): Concat[B :: A :: ev.T] = {
    Concat(ev.two(stack) :: ev.one(stack) :: ev.tail(stack))
  }

  // Numeric

  def *(implicit ev: IsBinary[Int, Int, L]): Concat[Int :: ev.T] = {
    Concat((ev.one(stack) * ev.two(stack)) :: ev.tail(stack))
  }

  def +(implicit ev: IsBinary[Int, Int, L]): Concat[Int :: ev.T] = {
    Concat((ev.one(stack) + ev.two(stack)) :: ev.tail(stack))
  }

  // Boolean

  def ifThen[A](implicit ev: IsTernary[Boolean, A, A, L]): Concat[A :: ev.T] = {
    Concat(
      if(ev.one(stack))
        ev.two(stack) :: ev.tail(stack)
      else
        ev.three(stack) :: ev.tail(stack)
    )
  }

}

// Type inference utilities
object Evidence {

  // Evidence that an HList contains at least two elements of type A
  // and B respectively.
  trait IsBinary[A, B, L <: HList] {
    type T <: HList

    def one(l : L) : A
    def two(l : L) : B
    def tail(l : L) : T
  }

  object IsBinary {
    def apply[A, B, L <: HList](implicit isBinary: IsBinary[A, B, L]): Aux[L, A, B, isBinary.T] =
      isBinary

    type Aux[L <: HList, A0, B0, T0 <: HList] =
      IsBinary[A0, B0, L] {
        type T = T0
      }
    implicit def hlistIsBinary[A0, B0, T0 <: HList]: Aux[A0 :: B0 :: T0, A0, B0, T0] =
      new IsBinary[A0, B0, A0 :: B0 :: T0] {
        type T = T0

        def one(l : A0 :: B0 :: T0) : A0 = l.head
        def two(l : A0 :: B0 :: T0) : B0 = l.tail.head
        def tail(l : A0 :: B0 :: T0) : T = l.tail.tail
      }
  }

  // Evidence that an HList contains three elements of type A, B, and
  // C respectively
  trait IsTernary[A, B, C, L <: HList] {
    type T <: HList

    def one(l : L) : A
    def two(l : L) : B
    def three(l : L) : C
    def tail(l : L) : T
  }

  object IsTernary {
    def apply[A, B, C, L <: HList](implicit isTernary: IsTernary[A, B, C, L]): Aux[L, A, B, C, isTernary.T] =
      isTernary

    type Aux[L <: HList, A, B, C, T0 <: HList] =
      IsTernary[A, B, C, L] {
        type T = T0
      }
    implicit def hlistIsTernary[A0, B0, C0, T0 <: HList]: Aux[A0 :: B0 :: C0 :: T0, A0, B0, C0, T0] =
      new IsTernary[A0, B0, C0, A0 :: B0 :: C0 :: T0] {
        type T = T0

        def one(l : A0 :: B0 :: C0 :: T0) : A0 = l.head
        def two(l : A0 :: B0 :: C0 :: T0) : B0 = l.tail.head
        def three(l : A0 :: B0 :: C0 :: T0) : C0 = l.tail.tail.head
        def tail(l : A0 :: B0 :: C0 :: T0) : T0 = l.tail.tail.tail
      }
  }

}
