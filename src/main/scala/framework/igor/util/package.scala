package framework.igor

/**
 * @author jda
 */
package object util {
  type Dim2[A[_], T] = A[A[T]]
  type Dim3[A[_], T] = A[A[A[T]]]
  type Dim4[A[_], T] = A[A[A[A[T]]]]
  type Dim5[A[_], T] = A[A[A[A[A[T]]]]]
  type Dim6[A[_], T] = A[A[A[A[A[A[T]]]]]]
}
