*> \brief \b ISAMAX
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       INTEGER FUNCTION ISAMAX(N,SX,INCX)
*
*       .. Scalar Arguments ..
*       INTEGER INCX,N
*       ..
*       .. Array Arguments ..
*       REAL SX(*)
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*>    ISAMAX finds the index of the first element having maximum absolute value.
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] N
*> \verbatim
*>          N is INTEGER
*>         number of elements in input vector(s)
*> \endverbatim
*>
*> \param[in] SX
*> \verbatim
*>          SX is REAL array, dimension ( 1 + ( N - 1 )*abs( INCX ) )
*> \endverbatim
*>
*> \param[in] INCX
*> \verbatim
*>          INCX is INTEGER
*>         storage spacing between elements of SX
*> \endverbatim
*
*  Authors:
*  ========
*
*> \author Univ. of Tennessee
*> \author Univ. of California Berkeley
*> \author Univ. of Colorado Denver
*> \author NAG Ltd.
*
*> \date November 2017
*
*> \ingroup aux_blas
*
*> \par Further Details:
*  =====================
*>
*> \verbatim
*>
*>     jack dongarra, linpack, 3/11/78.
*>     modified 3/93 to return if incx .le. 0.
*>     modified 12/3/93, array(1) declarations changed to array(*)
*> \endverbatim
*>
*  =====================================================================
      INTEGER FUNCTION ISAMAX(N,SX,INCX)
*
*  -- Reference BLAS level1 routine (version 3.8.0) --
*  -- Reference BLAS is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     November 2017
*
*     .. Scalar Arguments ..
      INTEGER INCX,N
*     ..
*     .. Array Arguments ..
      REAL SX(*)
*     ..
*
*  =====================================================================
*
*     .. Local Scalars ..
      REAL SMAX
      INTEGER I,IX
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC ABS
*     ..
      ISAMAX = 0
      IF (N.LT.1 .OR. INCX.LE.0) RETURN
      ISAMAX = 1
      IF (N.EQ.1) RETURN
      IF (INCX.EQ.1) THEN
*
*        code for increment equal to 1
*
         SMAX = ABS(SX(1))
         DO I = 2,N
            IF (ABS(SX(I)).GT.SMAX) THEN
               ISAMAX = I
               SMAX = ABS(SX(I))
            END IF
         END DO
      ELSE
*
*        code for increment not equal to 1
*
         IX = 1
         SMAX = ABS(SX(1))
         IX = IX + INCX
         DO I = 2,N
            IF (ABS(SX(IX)).GT.SMAX) THEN
               ISAMAX = I
               SMAX = ABS(SX(IX))
            END IF
            IX = IX + INCX
         END DO
      END IF
      RETURN
      END
*> \brief \b LSAME
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       LOGICAL FUNCTION LSAME(CA,CB)
*
*       .. Scalar Arguments ..
*       CHARACTER CA,CB
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> LSAME returns .TRUE. if CA is the same letter as CB regardless of
*> case.
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] CA
*> \verbatim
*>          CA is CHARACTER*1
*> \endverbatim
*>
*> \param[in] CB
*> \verbatim
*>          CB is CHARACTER*1
*>          CA and CB specify the single characters to be compared.
*> \endverbatim
*
*  Authors:
*  ========
*
*> \author Univ. of Tennessee
*> \author Univ. of California Berkeley
*> \author Univ. of Colorado Denver
*> \author NAG Ltd.
*
*> \date December 2016
*
*> \ingroup aux_blas
*
*  =====================================================================
      LOGICAL FUNCTION LSAME(CA,CB)
*
*  -- Reference BLAS level1 routine (version 3.1) --
*  -- Reference BLAS is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     December 2016
*
*     .. Scalar Arguments ..
      CHARACTER CA,CB
*     ..
*
* =====================================================================
*
*     .. Intrinsic Functions ..
      INTRINSIC ICHAR
*     ..
*     .. Local Scalars ..
      INTEGER INTA,INTB,ZCODE
*     ..
*
*     Test if the characters are equal
*
      LSAME = CA .EQ. CB
      IF (LSAME) RETURN
*
*     Now test for equivalence if both characters are alphabetic.
*
      ZCODE = ICHAR('Z')
*
*     Use 'Z' rather than 'A' so that ASCII can be detected on Prime
*     machines, on which ICHAR returns a value with bit 8 set.
*     ICHAR('A') on Prime machines returns 193 which is the same as
*     ICHAR('A') on an EBCDIC machine.
*
      INTA = ICHAR(CA)
      INTB = ICHAR(CB)
*
      IF (ZCODE.EQ.90 .OR. ZCODE.EQ.122) THEN
*
*        ASCII is assumed - ZCODE is the ASCII code of either lower or
*        upper case 'Z'.
*
          IF (INTA.GE.97 .AND. INTA.LE.122) INTA = INTA - 32
          IF (INTB.GE.97 .AND. INTB.LE.122) INTB = INTB - 32
*
      ELSE IF (ZCODE.EQ.233 .OR. ZCODE.EQ.169) THEN
*
*        EBCDIC is assumed - ZCODE is the EBCDIC code of either lower or
*        upper case 'Z'.
*
          IF (INTA.GE.129 .AND. INTA.LE.137 .OR.
     +        INTA.GE.145 .AND. INTA.LE.153 .OR.
     +        INTA.GE.162 .AND. INTA.LE.169) INTA = INTA + 64
          IF (INTB.GE.129 .AND. INTB.LE.137 .OR.
     +        INTB.GE.145 .AND. INTB.LE.153 .OR.
     +        INTB.GE.162 .AND. INTB.LE.169) INTB = INTB + 64
*
      ELSE IF (ZCODE.EQ.218 .OR. ZCODE.EQ.250) THEN
*
*        ASCII is assumed, on Prime machines - ZCODE is the ASCII code
*        plus 128 of either lower or upper case 'Z'.
*
          IF (INTA.GE.225 .AND. INTA.LE.250) INTA = INTA - 32
          IF (INTB.GE.225 .AND. INTB.LE.250) INTB = INTB - 32
      END IF
      LSAME = INTA .EQ. INTB
*
*     RETURN
*
*     End of LSAME
*
      END
*> \brief \b SGEMV
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE SGEMV(TRANS,M,N,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
*
*       .. Scalar Arguments ..
*       REAL ALPHA,BETA
*       INTEGER INCX,INCY,LDA,M,N
*       CHARACTER TRANS
*       ..
*       .. Array Arguments ..
*       REAL A(LDA,*),X(*),Y(*)
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> SGEMV  performs one of the matrix-vector operations
*>
*>    y := alpha*A*x + beta*y,   or   y := alpha*A**T*x + beta*y,
*>
*> where alpha and beta are scalars, x and y are vectors and A is an
*> m by n matrix.
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] TRANS
*> \verbatim
*>          TRANS is CHARACTER*1
*>           On entry, TRANS specifies the operation to be performed as
*>           follows:
*>
*>              TRANS = 'N' or 'n'   y := alpha*A*x + beta*y.
*>
*>              TRANS = 'T' or 't'   y := alpha*A**T*x + beta*y.
*>
*>              TRANS = 'C' or 'c'   y := alpha*A**T*x + beta*y.
*> \endverbatim
*>
*> \param[in] M
*> \verbatim
*>          M is INTEGER
*>           On entry, M specifies the number of rows of the matrix A.
*>           M must be at least zero.
*> \endverbatim
*>
*> \param[in] N
*> \verbatim
*>          N is INTEGER
*>           On entry, N specifies the number of columns of the matrix A.
*>           N must be at least zero.
*> \endverbatim
*>
*> \param[in] ALPHA
*> \verbatim
*>          ALPHA is REAL
*>           On entry, ALPHA specifies the scalar alpha.
*> \endverbatim
*>
*> \param[in] A
*> \verbatim
*>          A is REAL array, dimension ( LDA, N )
*>           Before entry, the leading m by n part of the array A must
*>           contain the matrix of coefficients.
*> \endverbatim
*>
*> \param[in] LDA
*> \verbatim
*>          LDA is INTEGER
*>           On entry, LDA specifies the first dimension of A as declared
*>           in the calling (sub) program. LDA must be at least
*>           max( 1, m ).
*> \endverbatim
*>
*> \param[in] X
*> \verbatim
*>          X is REAL array, dimension at least
*>           ( 1 + ( n - 1 )*abs( INCX ) ) when TRANS = 'N' or 'n'
*>           and at least
*>           ( 1 + ( m - 1 )*abs( INCX ) ) otherwise.
*>           Before entry, the incremented array X must contain the
*>           vector x.
*> \endverbatim
*>
*> \param[in] INCX
*> \verbatim
*>          INCX is INTEGER
*>           On entry, INCX specifies the increment for the elements of
*>           X. INCX must not be zero.
*> \endverbatim
*>
*> \param[in] BETA
*> \verbatim
*>          BETA is REAL
*>           On entry, BETA specifies the scalar beta. When BETA is
*>           supplied as zero then Y need not be set on input.
*> \endverbatim
*>
*> \param[in,out] Y
*> \verbatim
*>          Y is REAL array, dimension at least
*>           ( 1 + ( m - 1 )*abs( INCY ) ) when TRANS = 'N' or 'n'
*>           and at least
*>           ( 1 + ( n - 1 )*abs( INCY ) ) otherwise.
*>           Before entry with BETA non-zero, the incremented array Y
*>           must contain the vector y. On exit, Y is overwritten by the
*>           updated vector y.
*> \endverbatim
*>
*> \param[in] INCY
*> \verbatim
*>          INCY is INTEGER
*>           On entry, INCY specifies the increment for the elements of
*>           Y. INCY must not be zero.
*> \endverbatim
*
*  Authors:
*  ========
*
*> \author Univ. of Tennessee
*> \author Univ. of California Berkeley
*> \author Univ. of Colorado Denver
*> \author NAG Ltd.
*
*> \date December 2016
*
*> \ingroup single_blas_level2
*
*> \par Further Details:
*  =====================
*>
*> \verbatim
*>
*>  Level 2 Blas routine.
*>  The vector and matrix arguments are not referenced when N = 0, or M = 0
*>
*>  -- Written on 22-October-1986.
*>     Jack Dongarra, Argonne National Lab.
*>     Jeremy Du Croz, Nag Central Office.
*>     Sven Hammarling, Nag Central Office.
*>     Richard Hanson, Sandia National Labs.
*> \endverbatim
*>
*  =====================================================================
      SUBROUTINE SGEMV(TRANS,M,N,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
*
*  -- Reference BLAS level2 routine (version 3.7.0) --
*  -- Reference BLAS is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     December 2016
*
*     .. Scalar Arguments ..
      REAL ALPHA,BETA
      INTEGER INCX,INCY,LDA,M,N
      CHARACTER TRANS
*     ..
*     .. Array Arguments ..
      REAL A(LDA,*),X(*),Y(*)
*     ..
*
*  =====================================================================
*
*     .. Parameters ..
      REAL ONE,ZERO
      PARAMETER (ONE=1.0E+0,ZERO=0.0E+0)
*     ..
*     .. Local Scalars ..
      REAL TEMP
      INTEGER I,INFO,IX,IY,J,JX,JY,KX,KY,LENX,LENY
*     ..
*     .. External Functions ..
      LOGICAL LSAME
      EXTERNAL LSAME
*     ..
*     .. External Subroutines ..
      EXTERNAL XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC MAX
*     ..
*
*     Test the input parameters.
*
      INFO = 0
      IF (.NOT.LSAME(TRANS,'N') .AND. .NOT.LSAME(TRANS,'T') .AND.
     +    .NOT.LSAME(TRANS,'C')) THEN
          INFO = 1
      ELSE IF (M.LT.0) THEN
          INFO = 2
      ELSE IF (N.LT.0) THEN
          INFO = 3
      ELSE IF (LDA.LT.MAX(1,M)) THEN
          INFO = 6
      ELSE IF (INCX.EQ.0) THEN
          INFO = 8
      ELSE IF (INCY.EQ.0) THEN
          INFO = 11
      END IF
      IF (INFO.NE.0) THEN
          CALL XERBLA('SGEMV ',INFO)
          RETURN
      END IF
*
*     Quick return if possible.
*
      IF ((M.EQ.0) .OR. (N.EQ.0) .OR.
     +    ((ALPHA.EQ.ZERO).AND. (BETA.EQ.ONE))) RETURN
*
*     Set  LENX  and  LENY, the lengths of the vectors x and y, and set
*     up the start points in  X  and  Y.
*
      IF (LSAME(TRANS,'N')) THEN
          LENX = N
          LENY = M
      ELSE
          LENX = M
          LENY = N
      END IF
      IF (INCX.GT.0) THEN
          KX = 1
      ELSE
          KX = 1 - (LENX-1)*INCX
      END IF
      IF (INCY.GT.0) THEN
          KY = 1
      ELSE
          KY = 1 - (LENY-1)*INCY
      END IF
*
*     Start the operations. In this version the elements of A are
*     accessed sequentially with one pass through A.
*
*     First form  y := beta*y.
*
      IF (BETA.NE.ONE) THEN
          IF (INCY.EQ.1) THEN
              IF (BETA.EQ.ZERO) THEN
                  DO 10 I = 1,LENY
                      Y(I) = ZERO
   10             CONTINUE
              ELSE
                  DO 20 I = 1,LENY
                      Y(I) = BETA*Y(I)
   20             CONTINUE
              END IF
          ELSE
              IY = KY
              IF (BETA.EQ.ZERO) THEN
                  DO 30 I = 1,LENY
                      Y(IY) = ZERO
                      IY = IY + INCY
   30             CONTINUE
              ELSE
                  DO 40 I = 1,LENY
                      Y(IY) = BETA*Y(IY)
                      IY = IY + INCY
   40             CONTINUE
              END IF
          END IF
      END IF
      IF (ALPHA.EQ.ZERO) RETURN
      IF (LSAME(TRANS,'N')) THEN
*
*        Form  y := alpha*A*x + y.
*
          JX = KX
          IF (INCY.EQ.1) THEN
              DO 60 J = 1,N
                  TEMP = ALPHA*X(JX)
                  DO 50 I = 1,M
                      Y(I) = Y(I) + TEMP*A(I,J)
   50             CONTINUE
                  JX = JX + INCX
   60         CONTINUE
          ELSE
              DO 80 J = 1,N
                  TEMP = ALPHA*X(JX)
                  IY = KY
                  DO 70 I = 1,M
                      Y(IY) = Y(IY) + TEMP*A(I,J)
                      IY = IY + INCY
   70             CONTINUE
                  JX = JX + INCX
   80         CONTINUE
          END IF
      ELSE
*
*        Form  y := alpha*A**T*x + y.
*
          JY = KY
          IF (INCX.EQ.1) THEN
              DO 100 J = 1,N
                  TEMP = ZERO
                  DO 90 I = 1,M
                      TEMP = TEMP + A(I,J)*X(I)
   90             CONTINUE
                  Y(JY) = Y(JY) + ALPHA*TEMP
                  JY = JY + INCY
  100         CONTINUE
          ELSE
              DO 120 J = 1,N
                  TEMP = ZERO
                  IX = KX
                  DO 110 I = 1,M
                      TEMP = TEMP + A(I,J)*X(IX)
                      IX = IX + INCX
  110             CONTINUE
                  Y(JY) = Y(JY) + ALPHA*TEMP
                  JY = JY + INCY
  120         CONTINUE
          END IF
      END IF
*
      RETURN
*
*     End of SGEMV .
*
      END
*> \brief \b SGER
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE SGER(M,N,ALPHA,X,INCX,Y,INCY,A,LDA)
*
*       .. Scalar Arguments ..
*       REAL ALPHA
*       INTEGER INCX,INCY,LDA,M,N
*       ..
*       .. Array Arguments ..
*       REAL A(LDA,*),X(*),Y(*)
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> SGER   performs the rank 1 operation
*>
*>    A := alpha*x*y**T + A,
*>
*> where alpha is a scalar, x is an m element vector, y is an n element
*> vector and A is an m by n matrix.
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] M
*> \verbatim
*>          M is INTEGER
*>           On entry, M specifies the number of rows of the matrix A.
*>           M must be at least zero.
*> \endverbatim
*>
*> \param[in] N
*> \verbatim
*>          N is INTEGER
*>           On entry, N specifies the number of columns of the matrix A.
*>           N must be at least zero.
*> \endverbatim
*>
*> \param[in] ALPHA
*> \verbatim
*>          ALPHA is REAL
*>           On entry, ALPHA specifies the scalar alpha.
*> \endverbatim
*>
*> \param[in] X
*> \verbatim
*>          X is REAL array, dimension at least
*>           ( 1 + ( m - 1 )*abs( INCX ) ).
*>           Before entry, the incremented array X must contain the m
*>           element vector x.
*> \endverbatim
*>
*> \param[in] INCX
*> \verbatim
*>          INCX is INTEGER
*>           On entry, INCX specifies the increment for the elements of
*>           X. INCX must not be zero.
*> \endverbatim
*>
*> \param[in] Y
*> \verbatim
*>          Y is REAL array, dimension at least
*>           ( 1 + ( n - 1 )*abs( INCY ) ).
*>           Before entry, the incremented array Y must contain the n
*>           element vector y.
*> \endverbatim
*>
*> \param[in] INCY
*> \verbatim
*>          INCY is INTEGER
*>           On entry, INCY specifies the increment for the elements of
*>           Y. INCY must not be zero.
*> \endverbatim
*>
*> \param[in,out] A
*> \verbatim
*>          A is REAL array, dimension ( LDA, N )
*>           Before entry, the leading m by n part of the array A must
*>           contain the matrix of coefficients. On exit, A is
*>           overwritten by the updated matrix.
*> \endverbatim
*>
*> \param[in] LDA
*> \verbatim
*>          LDA is INTEGER
*>           On entry, LDA specifies the first dimension of A as declared
*>           in the calling (sub) program. LDA must be at least
*>           max( 1, m ).
*> \endverbatim
*
*  Authors:
*  ========
*
*> \author Univ. of Tennessee
*> \author Univ. of California Berkeley
*> \author Univ. of Colorado Denver
*> \author NAG Ltd.
*
*> \date December 2016
*
*> \ingroup single_blas_level2
*
*> \par Further Details:
*  =====================
*>
*> \verbatim
*>
*>  Level 2 Blas routine.
*>
*>  -- Written on 22-October-1986.
*>     Jack Dongarra, Argonne National Lab.
*>     Jeremy Du Croz, Nag Central Office.
*>     Sven Hammarling, Nag Central Office.
*>     Richard Hanson, Sandia National Labs.
*> \endverbatim
*>
*  =====================================================================
      SUBROUTINE SGER(M,N,ALPHA,X,INCX,Y,INCY,A,LDA)
*
*  -- Reference BLAS level2 routine (version 3.7.0) --
*  -- Reference BLAS is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     December 2016
*
*     .. Scalar Arguments ..
      REAL ALPHA
      INTEGER INCX,INCY,LDA,M,N
*     ..
*     .. Array Arguments ..
      REAL A(LDA,*),X(*),Y(*)
*     ..
*
*  =====================================================================
*
*     .. Parameters ..
      REAL ZERO
      PARAMETER (ZERO=0.0E+0)
*     ..
*     .. Local Scalars ..
      REAL TEMP
      INTEGER I,INFO,IX,J,JY,KX
*     ..
*     .. External Subroutines ..
      EXTERNAL XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC MAX
*     ..
*
*     Test the input parameters.
*
      INFO = 0
      IF (M.LT.0) THEN
          INFO = 1
      ELSE IF (N.LT.0) THEN
          INFO = 2
      ELSE IF (INCX.EQ.0) THEN
          INFO = 5
      ELSE IF (INCY.EQ.0) THEN
          INFO = 7
      ELSE IF (LDA.LT.MAX(1,M)) THEN
          INFO = 9
      END IF
      IF (INFO.NE.0) THEN
          CALL XERBLA('SGER  ',INFO)
          RETURN
      END IF
*
*     Quick return if possible.
*
      IF ((M.EQ.0) .OR. (N.EQ.0) .OR. (ALPHA.EQ.ZERO)) RETURN
*
*     Start the operations. In this version the elements of A are
*     accessed sequentially with one pass through A.
*
      IF (INCY.GT.0) THEN
          JY = 1
      ELSE
          JY = 1 - (N-1)*INCY
      END IF
      IF (INCX.EQ.1) THEN
          DO 20 J = 1,N
              IF (Y(JY).NE.ZERO) THEN
                  TEMP = ALPHA*Y(JY)
                  DO 10 I = 1,M
                      A(I,J) = A(I,J) + X(I)*TEMP
   10             CONTINUE
              END IF
              JY = JY + INCY
   20     CONTINUE
      ELSE
          IF (INCX.GT.0) THEN
              KX = 1
          ELSE
              KX = 1 - (M-1)*INCX
          END IF
          DO 40 J = 1,N
              IF (Y(JY).NE.ZERO) THEN
                  TEMP = ALPHA*Y(JY)
                  IX = KX
                  DO 30 I = 1,M
                      A(I,J) = A(I,J) + X(IX)*TEMP
                      IX = IX + INCX
   30             CONTINUE
              END IF
              JY = JY + INCY
   40     CONTINUE
      END IF
*
      RETURN
*
*     End of SGER  .
*
      END
*> \brief \b SSCAL
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE SSCAL(N,SA,SX,INCX)
*
*       .. Scalar Arguments ..
*       REAL SA
*       INTEGER INCX,N
*       ..
*       .. Array Arguments ..
*       REAL SX(*)
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*>    SSCAL scales a vector by a constant.
*>    uses unrolled loops for increment equal to 1.
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] N
*> \verbatim
*>          N is INTEGER
*>         number of elements in input vector(s)
*> \endverbatim
*>
*> \param[in] SA
*> \verbatim
*>          SA is REAL
*>           On entry, SA specifies the scalar alpha.
*> \endverbatim
*>
*> \param[in,out] SX
*> \verbatim
*>          SX is REAL array, dimension ( 1 + ( N - 1 )*abs( INCX ) )
*> \endverbatim
*>
*> \param[in] INCX
*> \verbatim
*>          INCX is INTEGER
*>         storage spacing between elements of SX
*> \endverbatim
*
*  Authors:
*  ========
*
*> \author Univ. of Tennessee
*> \author Univ. of California Berkeley
*> \author Univ. of Colorado Denver
*> \author NAG Ltd.
*
*> \date November 2017
*
*> \ingroup single_blas_level1
*
*> \par Further Details:
*  =====================
*>
*> \verbatim
*>
*>     jack dongarra, linpack, 3/11/78.
*>     modified 3/93 to return if incx .le. 0.
*>     modified 12/3/93, array(1) declarations changed to array(*)
*> \endverbatim
*>
*  =====================================================================
      SUBROUTINE SSCAL(N,SA,SX,INCX)
*
*  -- Reference BLAS level1 routine (version 3.8.0) --
*  -- Reference BLAS is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     November 2017
*
*     .. Scalar Arguments ..
      REAL SA
      INTEGER INCX,N
*     ..
*     .. Array Arguments ..
      REAL SX(*)
*     ..
*
*  =====================================================================
*
*     .. Local Scalars ..
      INTEGER I,M,MP1,NINCX
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC MOD
*     ..
      IF (N.LE.0 .OR. INCX.LE.0) RETURN
      IF (INCX.EQ.1) THEN
*
*        code for increment equal to 1
*
*
*        clean-up loop
*
         M = MOD(N,5)
         IF (M.NE.0) THEN
            DO I = 1,M
               SX(I) = SA*SX(I)
            END DO
            IF (N.LT.5) RETURN
         END IF
         MP1 = M + 1
         DO I = MP1,N,5
            SX(I) = SA*SX(I)
            SX(I+1) = SA*SX(I+1)
            SX(I+2) = SA*SX(I+2)
            SX(I+3) = SA*SX(I+3)
            SX(I+4) = SA*SX(I+4)
         END DO
      ELSE
*
*        code for increment not equal to 1
*
         NINCX = N*INCX
         DO I = 1,NINCX,INCX
            SX(I) = SA*SX(I)
         END DO
      END IF
      RETURN
      END
*> \brief \b SSPR
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE SSPR(UPLO,N,ALPHA,X,INCX,AP)
*
*       .. Scalar Arguments ..
*       REAL ALPHA
*       INTEGER INCX,N
*       CHARACTER UPLO
*       ..
*       .. Array Arguments ..
*       REAL AP(*),X(*)
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> SSPR    performs the symmetric rank 1 operation
*>
*>    A := alpha*x*x**T + A,
*>
*> where alpha is a real scalar, x is an n element vector and A is an
*> n by n symmetric matrix, supplied in packed form.
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] UPLO
*> \verbatim
*>          UPLO is CHARACTER*1
*>           On entry, UPLO specifies whether the upper or lower
*>           triangular part of the matrix A is supplied in the packed
*>           array AP as follows:
*>
*>              UPLO = 'U' or 'u'   The upper triangular part of A is
*>                                  supplied in AP.
*>
*>              UPLO = 'L' or 'l'   The lower triangular part of A is
*>                                  supplied in AP.
*> \endverbatim
*>
*> \param[in] N
*> \verbatim
*>          N is INTEGER
*>           On entry, N specifies the order of the matrix A.
*>           N must be at least zero.
*> \endverbatim
*>
*> \param[in] ALPHA
*> \verbatim
*>          ALPHA is REAL
*>           On entry, ALPHA specifies the scalar alpha.
*> \endverbatim
*>
*> \param[in] X
*> \verbatim
*>          X is REAL array, dimension at least
*>           ( 1 + ( n - 1 )*abs( INCX ) ).
*>           Before entry, the incremented array X must contain the n
*>           element vector x.
*> \endverbatim
*>
*> \param[in] INCX
*> \verbatim
*>          INCX is INTEGER
*>           On entry, INCX specifies the increment for the elements of
*>           X. INCX must not be zero.
*> \endverbatim
*>
*> \param[in,out] AP
*> \verbatim
*>          AP is REAL array, dimension at least
*>           ( ( n*( n + 1 ) )/2 ).
*>           Before entry with  UPLO = 'U' or 'u', the array AP must
*>           contain the upper triangular part of the symmetric matrix
*>           packed sequentially, column by column, so that AP( 1 )
*>           contains a( 1, 1 ), AP( 2 ) and AP( 3 ) contain a( 1, 2 )
*>           and a( 2, 2 ) respectively, and so on. On exit, the array
*>           AP is overwritten by the upper triangular part of the
*>           updated matrix.
*>           Before entry with UPLO = 'L' or 'l', the array AP must
*>           contain the lower triangular part of the symmetric matrix
*>           packed sequentially, column by column, so that AP( 1 )
*>           contains a( 1, 1 ), AP( 2 ) and AP( 3 ) contain a( 2, 1 )
*>           and a( 3, 1 ) respectively, and so on. On exit, the array
*>           AP is overwritten by the lower triangular part of the
*>           updated matrix.
*> \endverbatim
*
*  Authors:
*  ========
*
*> \author Univ. of Tennessee
*> \author Univ. of California Berkeley
*> \author Univ. of Colorado Denver
*> \author NAG Ltd.
*
*> \date December 2016
*
*> \ingroup single_blas_level2
*
*> \par Further Details:
*  =====================
*>
*> \verbatim
*>
*>  Level 2 Blas routine.
*>
*>  -- Written on 22-October-1986.
*>     Jack Dongarra, Argonne National Lab.
*>     Jeremy Du Croz, Nag Central Office.
*>     Sven Hammarling, Nag Central Office.
*>     Richard Hanson, Sandia National Labs.
*> \endverbatim
*>
*  =====================================================================
      SUBROUTINE SSPR(UPLO,N,ALPHA,X,INCX,AP)
*
*  -- Reference BLAS level2 routine (version 3.7.0) --
*  -- Reference BLAS is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     December 2016
*
*     .. Scalar Arguments ..
      REAL ALPHA
      INTEGER INCX,N
      CHARACTER UPLO
*     ..
*     .. Array Arguments ..
      REAL AP(*),X(*)
*     ..
*
*  =====================================================================
*
*     .. Parameters ..
      REAL ZERO
      PARAMETER (ZERO=0.0E+0)
*     ..
*     .. Local Scalars ..
      REAL TEMP
      INTEGER I,INFO,IX,J,JX,K,KK,KX
*     ..
*     .. External Functions ..
      LOGICAL LSAME
      EXTERNAL LSAME
*     ..
*     .. External Subroutines ..
      EXTERNAL XERBLA
*     ..
*
*     Test the input parameters.
*
      INFO = 0
      IF (.NOT.LSAME(UPLO,'U') .AND. .NOT.LSAME(UPLO,'L')) THEN
          INFO = 1
      ELSE IF (N.LT.0) THEN
          INFO = 2
      ELSE IF (INCX.EQ.0) THEN
          INFO = 5
      END IF
      IF (INFO.NE.0) THEN
          CALL XERBLA('SSPR  ',INFO)
          RETURN
      END IF
*
*     Quick return if possible.
*
      IF ((N.EQ.0) .OR. (ALPHA.EQ.ZERO)) RETURN
*
*     Set the start point in X if the increment is not unity.
*
      IF (INCX.LE.0) THEN
          KX = 1 - (N-1)*INCX
      ELSE IF (INCX.NE.1) THEN
          KX = 1
      END IF
*
*     Start the operations. In this version the elements of the array AP
*     are accessed sequentially with one pass through AP.
*
      KK = 1
      IF (LSAME(UPLO,'U')) THEN
*
*        Form  A  when upper triangle is stored in AP.
*
          IF (INCX.EQ.1) THEN
              DO 20 J = 1,N
                  IF (X(J).NE.ZERO) THEN
                      TEMP = ALPHA*X(J)
                      K = KK
                      DO 10 I = 1,J
                          AP(K) = AP(K) + X(I)*TEMP
                          K = K + 1
   10                 CONTINUE
                  END IF
                  KK = KK + J
   20         CONTINUE
          ELSE
              JX = KX
              DO 40 J = 1,N
                  IF (X(JX).NE.ZERO) THEN
                      TEMP = ALPHA*X(JX)
                      IX = KX
                      DO 30 K = KK,KK + J - 1
                          AP(K) = AP(K) + X(IX)*TEMP
                          IX = IX + INCX
   30                 CONTINUE
                  END IF
                  JX = JX + INCX
                  KK = KK + J
   40         CONTINUE
          END IF
      ELSE
*
*        Form  A  when lower triangle is stored in AP.
*
          IF (INCX.EQ.1) THEN
              DO 60 J = 1,N
                  IF (X(J).NE.ZERO) THEN
                      TEMP = ALPHA*X(J)
                      K = KK
                      DO 50 I = J,N
                          AP(K) = AP(K) + X(I)*TEMP
                          K = K + 1
   50                 CONTINUE
                  END IF
                  KK = KK + N - J + 1
   60         CONTINUE
          ELSE
              JX = KX
              DO 80 J = 1,N
                  IF (X(JX).NE.ZERO) THEN
                      TEMP = ALPHA*X(JX)
                      IX = JX
                      DO 70 K = KK,KK + N - J
                          AP(K) = AP(K) + X(IX)*TEMP
                          IX = IX + INCX
   70                 CONTINUE
                  END IF
                  JX = JX + INCX
                  KK = KK + N - J + 1
   80         CONTINUE
          END IF
      END IF
*
      RETURN
*
*     End of SSPR  .
*
      END
*> \brief <b> SSPSV computes the solution to system of linear equations A * X = B for OTHER matrices</b>
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*> \htmlonly
*> Download SSPSV + dependencies
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.tgz?format=tgz&filename=/lapack/lapack_routine/sspsv.f">
*> [TGZ]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.zip?format=zip&filename=/lapack/lapack_routine/sspsv.f">
*> [ZIP]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.txt?format=txt&filename=/lapack/lapack_routine/sspsv.f">
*> [TXT]</a>
*> \endhtmlonly
*
*  Definition:
*  ===========
*
*       SUBROUTINE SSPSV( UPLO, N, NRHS, AP, IPIV, B, LDB, INFO )
*
*       .. Scalar Arguments ..
*       CHARACTER          UPLO
*       INTEGER            INFO, LDB, N, NRHS
*       ..
*       .. Array Arguments ..
*       INTEGER            IPIV( * )
*       REAL               AP( * ), B( LDB, * )
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> SSPSV computes the solution to a real system of linear equations
*>    A * X = B,
*> where A is an N-by-N symmetric matrix stored in packed format and X
*> and B are N-by-NRHS matrices.
*>
*> The diagonal pivoting method is used to factor A as
*>    A = U * D * U**T,  if UPLO = 'U', or
*>    A = L * D * L**T,  if UPLO = 'L',
*> where U (or L) is a product of permutation and unit upper (lower)
*> triangular matrices, D is symmetric and block diagonal with 1-by-1
*> and 2-by-2 diagonal blocks.  The factored form of A is then used to
*> solve the system of equations A * X = B.
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] UPLO
*> \verbatim
*>          UPLO is CHARACTER*1
*>          = 'U':  Upper triangle of A is stored;
*>          = 'L':  Lower triangle of A is stored.
*> \endverbatim
*>
*> \param[in] N
*> \verbatim
*>          N is INTEGER
*>          The number of linear equations, i.e., the order of the
*>          matrix A.  N >= 0.
*> \endverbatim
*>
*> \param[in] NRHS
*> \verbatim
*>          NRHS is INTEGER
*>          The number of right hand sides, i.e., the number of columns
*>          of the matrix B.  NRHS >= 0.
*> \endverbatim
*>
*> \param[in,out] AP
*> \verbatim
*>          AP is REAL array, dimension (N*(N+1)/2)
*>          On entry, the upper or lower triangle of the symmetric matrix
*>          A, packed columnwise in a linear array.  The j-th column of A
*>          is stored in the array AP as follows:
*>          if UPLO = 'U', AP(i + (j-1)*j/2) = A(i,j) for 1<=i<=j;
*>          if UPLO = 'L', AP(i + (j-1)*(2n-j)/2) = A(i,j) for j<=i<=n.
*>          See below for further details.
*>
*>          On exit, the block diagonal matrix D and the multipliers used
*>          to obtain the factor U or L from the factorization
*>          A = U*D*U**T or A = L*D*L**T as computed by SSPTRF, stored as
*>          a packed triangular matrix in the same storage format as A.
*> \endverbatim
*>
*> \param[out] IPIV
*> \verbatim
*>          IPIV is INTEGER array, dimension (N)
*>          Details of the interchanges and the block structure of D, as
*>          determined by SSPTRF.  If IPIV(k) > 0, then rows and columns
*>          k and IPIV(k) were interchanged, and D(k,k) is a 1-by-1
*>          diagonal block.  If UPLO = 'U' and IPIV(k) = IPIV(k-1) < 0,
*>          then rows and columns k-1 and -IPIV(k) were interchanged and
*>          D(k-1:k,k-1:k) is a 2-by-2 diagonal block.  If UPLO = 'L' and
*>          IPIV(k) = IPIV(k+1) < 0, then rows and columns k+1 and
*>          -IPIV(k) were interchanged and D(k:k+1,k:k+1) is a 2-by-2
*>          diagonal block.
*> \endverbatim
*>
*> \param[in,out] B
*> \verbatim
*>          B is REAL array, dimension (LDB,NRHS)
*>          On entry, the N-by-NRHS right hand side matrix B.
*>          On exit, if INFO = 0, the N-by-NRHS solution matrix X.
*> \endverbatim
*>
*> \param[in] LDB
*> \verbatim
*>          LDB is INTEGER
*>          The leading dimension of the array B.  LDB >= max(1,N).
*> \endverbatim
*>
*> \param[out] INFO
*> \verbatim
*>          INFO is INTEGER
*>          = 0:  successful exit
*>          < 0:  if INFO = -i, the i-th argument had an illegal value
*>          > 0:  if INFO = i, D(i,i) is exactly zero.  The factorization
*>                has been completed, but the block diagonal matrix D is
*>                exactly singular, so the solution could not be
*>                computed.
*> \endverbatim
*
*  Authors:
*  ========
*
*> \author Univ. of Tennessee
*> \author Univ. of California Berkeley
*> \author Univ. of Colorado Denver
*> \author NAG Ltd.
*
*> \date December 2016
*
*> \ingroup realOTHERsolve
*
*> \par Further Details:
*  =====================
*>
*> \verbatim
*>
*>  The packed storage scheme is illustrated by the following example
*>  when N = 4, UPLO = 'U':
*>
*>  Two-dimensional storage of the symmetric matrix A:
*>
*>     a11 a12 a13 a14
*>         a22 a23 a24
*>             a33 a34     (aij = aji)
*>                 a44
*>
*>  Packed storage of the upper triangle of A:
*>
*>  AP = [ a11, a12, a22, a13, a23, a33, a14, a24, a34, a44 ]
*> \endverbatim
*>
*  =====================================================================
      SUBROUTINE SSPSV( UPLO, N, NRHS, AP, IPIV, B, LDB, INFO )
*
*  -- LAPACK driver routine (version 3.7.0) --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     December 2016
*
*     .. Scalar Arguments ..
      CHARACTER          UPLO
      INTEGER            INFO, LDB, N, NRHS
*     ..
*     .. Array Arguments ..
      INTEGER            IPIV( * )
      REAL               AP( * ), B( LDB, * )
*     ..
*
*  =====================================================================
*
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. External Subroutines ..
      EXTERNAL           SSPTRF, SSPTRS, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
      IF( .NOT.LSAME( UPLO, 'U' ) .AND. .NOT.LSAME( UPLO, 'L' ) ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( NRHS.LT.0 ) THEN
         INFO = -3
      ELSE IF( LDB.LT.MAX( 1, N ) ) THEN
         INFO = -7
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'SSPSV ', -INFO )
         RETURN
      END IF
*
*     Compute the factorization A = U*D*U**T or A = L*D*L**T.
*
      CALL SSPTRF( UPLO, N, AP, IPIV, INFO )
      IF( INFO.EQ.0 ) THEN
*
*        Solve the system A*X = B, overwriting B with X.
*
         CALL SSPTRS( UPLO, N, NRHS, AP, IPIV, B, LDB, INFO )
*
      END IF
      RETURN
*
*     End of SSPSV
*
      END
*> \brief \b SSPTRF
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*> \htmlonly
*> Download SSPTRF + dependencies
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.tgz?format=tgz&filename=/lapack/lapack_routine/ssptrf.f">
*> [TGZ]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.zip?format=zip&filename=/lapack/lapack_routine/ssptrf.f">
*> [ZIP]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.txt?format=txt&filename=/lapack/lapack_routine/ssptrf.f">
*> [TXT]</a>
*> \endhtmlonly
*
*  Definition:
*  ===========
*
*       SUBROUTINE SSPTRF( UPLO, N, AP, IPIV, INFO )
*
*       .. Scalar Arguments ..
*       CHARACTER          UPLO
*       INTEGER            INFO, N
*       ..
*       .. Array Arguments ..
*       INTEGER            IPIV( * )
*       REAL               AP( * )
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> SSPTRF computes the factorization of a real symmetric matrix A stored
*> in packed format using the Bunch-Kaufman diagonal pivoting method:
*>
*>    A = U*D*U**T  or  A = L*D*L**T
*>
*> where U (or L) is a product of permutation and unit upper (lower)
*> triangular matrices, and D is symmetric and block diagonal with
*> 1-by-1 and 2-by-2 diagonal blocks.
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] UPLO
*> \verbatim
*>          UPLO is CHARACTER*1
*>          = 'U':  Upper triangle of A is stored;
*>          = 'L':  Lower triangle of A is stored.
*> \endverbatim
*>
*> \param[in] N
*> \verbatim
*>          N is INTEGER
*>          The order of the matrix A.  N >= 0.
*> \endverbatim
*>
*> \param[in,out] AP
*> \verbatim
*>          AP is REAL array, dimension (N*(N+1)/2)
*>          On entry, the upper or lower triangle of the symmetric matrix
*>          A, packed columnwise in a linear array.  The j-th column of A
*>          is stored in the array AP as follows:
*>          if UPLO = 'U', AP(i + (j-1)*j/2) = A(i,j) for 1<=i<=j;
*>          if UPLO = 'L', AP(i + (j-1)*(2n-j)/2) = A(i,j) for j<=i<=n.
*>
*>          On exit, the block diagonal matrix D and the multipliers used
*>          to obtain the factor U or L, stored as a packed triangular
*>          matrix overwriting A (see below for further details).
*> \endverbatim
*>
*> \param[out] IPIV
*> \verbatim
*>          IPIV is INTEGER array, dimension (N)
*>          Details of the interchanges and the block structure of D.
*>          If IPIV(k) > 0, then rows and columns k and IPIV(k) were
*>          interchanged and D(k,k) is a 1-by-1 diagonal block.
*>          If UPLO = 'U' and IPIV(k) = IPIV(k-1) < 0, then rows and
*>          columns k-1 and -IPIV(k) were interchanged and D(k-1:k,k-1:k)
*>          is a 2-by-2 diagonal block.  If UPLO = 'L' and IPIV(k) =
*>          IPIV(k+1) < 0, then rows and columns k+1 and -IPIV(k) were
*>          interchanged and D(k:k+1,k:k+1) is a 2-by-2 diagonal block.
*> \endverbatim
*>
*> \param[out] INFO
*> \verbatim
*>          INFO is INTEGER
*>          = 0: successful exit
*>          < 0: if INFO = -i, the i-th argument had an illegal value
*>          > 0: if INFO = i, D(i,i) is exactly zero.  The factorization
*>               has been completed, but the block diagonal matrix D is
*>               exactly singular, and division by zero will occur if it
*>               is used to solve a system of equations.
*> \endverbatim
*
*  Authors:
*  ========
*
*> \author Univ. of Tennessee
*> \author Univ. of California Berkeley
*> \author Univ. of Colorado Denver
*> \author NAG Ltd.
*
*> \date December 2016
*
*> \ingroup realOTHERcomputational
*
*> \par Further Details:
*  =====================
*>
*> \verbatim
*>
*>  5-96 - Based on modifications by J. Lewis, Boeing Computer Services
*>         Company
*>
*>  If UPLO = 'U', then A = U*D*U**T, where
*>     U = P(n)*U(n)* ... *P(k)U(k)* ...,
*>  i.e., U is a product of terms P(k)*U(k), where k decreases from n to
*>  1 in steps of 1 or 2, and D is a block diagonal matrix with 1-by-1
*>  and 2-by-2 diagonal blocks D(k).  P(k) is a permutation matrix as
*>  defined by IPIV(k), and U(k) is a unit upper triangular matrix, such
*>  that if the diagonal block D(k) is of order s (s = 1 or 2), then
*>
*>             (   I    v    0   )   k-s
*>     U(k) =  (   0    I    0   )   s
*>             (   0    0    I   )   n-k
*>                k-s   s   n-k
*>
*>  If s = 1, D(k) overwrites A(k,k), and v overwrites A(1:k-1,k).
*>  If s = 2, the upper triangle of D(k) overwrites A(k-1,k-1), A(k-1,k),
*>  and A(k,k), and v overwrites A(1:k-2,k-1:k).
*>
*>  If UPLO = 'L', then A = L*D*L**T, where
*>     L = P(1)*L(1)* ... *P(k)*L(k)* ...,
*>  i.e., L is a product of terms P(k)*L(k), where k increases from 1 to
*>  n in steps of 1 or 2, and D is a block diagonal matrix with 1-by-1
*>  and 2-by-2 diagonal blocks D(k).  P(k) is a permutation matrix as
*>  defined by IPIV(k), and L(k) is a unit lower triangular matrix, such
*>  that if the diagonal block D(k) is of order s (s = 1 or 2), then
*>
*>             (   I    0     0   )  k-1
*>     L(k) =  (   0    I     0   )  s
*>             (   0    v     I   )  n-k-s+1
*>                k-1   s  n-k-s+1
*>
*>  If s = 1, D(k) overwrites A(k,k), and v overwrites A(k+1:n,k).
*>  If s = 2, the lower triangle of D(k) overwrites A(k,k), A(k+1,k),
*>  and A(k+1,k+1), and v overwrites A(k+2:n,k:k+1).
*> \endverbatim
*>
*  =====================================================================
      SUBROUTINE SSPTRF( UPLO, N, AP, IPIV, INFO )
*
*  -- LAPACK computational routine (version 3.7.0) --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     December 2016
*
*     .. Scalar Arguments ..
      CHARACTER          UPLO
      INTEGER            INFO, N
*     ..
*     .. Array Arguments ..
      INTEGER            IPIV( * )
      REAL               AP( * )
*     ..
*
*  =====================================================================
*
*     .. Parameters ..
      REAL               ZERO, ONE
      PARAMETER          ( ZERO = 0.0E+0, ONE = 1.0E+0 )
      REAL               EIGHT, SEVTEN
      PARAMETER          ( EIGHT = 8.0E+0, SEVTEN = 17.0E+0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            UPPER
      INTEGER            I, IMAX, J, JMAX, K, KC, KK, KNC, KP, KPC,
     $                   KSTEP, KX, NPP
      REAL               ABSAKK, ALPHA, COLMAX, D11, D12, D21, D22, R1,
     $                   ROWMAX, T, WK, WKM1, WKP1
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            ISAMAX
      EXTERNAL           LSAME, ISAMAX
*     ..
*     .. External Subroutines ..
      EXTERNAL           SSCAL, SSPR, SSWAP, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, SQRT
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
      UPPER = LSAME( UPLO, 'U' )
      IF( .NOT.UPPER .AND. .NOT.LSAME( UPLO, 'L' ) ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'SSPTRF', -INFO )
         RETURN
      END IF
*
*     Initialize ALPHA for use in choosing pivot block size.
*
      ALPHA = ( ONE+SQRT( SEVTEN ) ) / EIGHT
*
      IF( UPPER ) THEN
*
*        Factorize A as U*D*U**T using the upper triangle of A
*
*        K is the main loop index, decreasing from N to 1 in steps of
*        1 or 2
*
         K = N
         KC = ( N-1 )*N / 2 + 1
   10    CONTINUE
         KNC = KC
*
*        If K < 1, exit from loop
*
         IF( K.LT.1 )
     $      GO TO 110
         KSTEP = 1
*
*        Determine rows and columns to be interchanged and whether
*        a 1-by-1 or 2-by-2 pivot block will be used
*
         ABSAKK = ABS( AP( KC+K-1 ) )
*
*        IMAX is the row-index of the largest off-diagonal element in
*        column K, and COLMAX is its absolute value
*
         IF( K.GT.1 ) THEN
            IMAX = ISAMAX( K-1, AP( KC ), 1 )
            COLMAX = ABS( AP( KC+IMAX-1 ) )
         ELSE
            COLMAX = ZERO
         END IF
*
         IF( MAX( ABSAKK, COLMAX ).EQ.ZERO ) THEN
*
*           Column K is zero: set INFO and continue
*
            IF( INFO.EQ.0 )
     $         INFO = K
            KP = K
         ELSE
            IF( ABSAKK.GE.ALPHA*COLMAX ) THEN
*
*              no interchange, use 1-by-1 pivot block
*
               KP = K
            ELSE
*
               ROWMAX = ZERO
               JMAX = IMAX
               KX = IMAX*( IMAX+1 ) / 2 + IMAX
               DO 20 J = IMAX + 1, K
                  IF( ABS( AP( KX ) ).GT.ROWMAX ) THEN
                     ROWMAX = ABS( AP( KX ) )
                     JMAX = J
                  END IF
                  KX = KX + J
   20          CONTINUE
               KPC = ( IMAX-1 )*IMAX / 2 + 1
               IF( IMAX.GT.1 ) THEN
                  JMAX = ISAMAX( IMAX-1, AP( KPC ), 1 )
                  ROWMAX = MAX( ROWMAX, ABS( AP( KPC+JMAX-1 ) ) )
               END IF
*
               IF( ABSAKK.GE.ALPHA*COLMAX*( COLMAX / ROWMAX ) ) THEN
*
*                 no interchange, use 1-by-1 pivot block
*
                  KP = K
               ELSE IF( ABS( AP( KPC+IMAX-1 ) ).GE.ALPHA*ROWMAX ) THEN
*
*                 interchange rows and columns K and IMAX, use 1-by-1
*                 pivot block
*
                  KP = IMAX
               ELSE
*
*                 interchange rows and columns K-1 and IMAX, use 2-by-2
*                 pivot block
*
                  KP = IMAX
                  KSTEP = 2
               END IF
            END IF
*
            KK = K - KSTEP + 1
            IF( KSTEP.EQ.2 )
     $         KNC = KNC - K + 1
            IF( KP.NE.KK ) THEN
*
*              Interchange rows and columns KK and KP in the leading
*              submatrix A(1:k,1:k)
*
               CALL SSWAP( KP-1, AP( KNC ), 1, AP( KPC ), 1 )
               KX = KPC + KP - 1
               DO 30 J = KP + 1, KK - 1
                  KX = KX + J - 1
                  T = AP( KNC+J-1 )
                  AP( KNC+J-1 ) = AP( KX )
                  AP( KX ) = T
   30          CONTINUE
               T = AP( KNC+KK-1 )
               AP( KNC+KK-1 ) = AP( KPC+KP-1 )
               AP( KPC+KP-1 ) = T
               IF( KSTEP.EQ.2 ) THEN
                  T = AP( KC+K-2 )
                  AP( KC+K-2 ) = AP( KC+KP-1 )
                  AP( KC+KP-1 ) = T
               END IF
            END IF
*
*           Update the leading submatrix
*
            IF( KSTEP.EQ.1 ) THEN
*
*              1-by-1 pivot block D(k): column k now holds
*
*              W(k) = U(k)*D(k)
*
*              where U(k) is the k-th column of U
*
*              Perform a rank-1 update of A(1:k-1,1:k-1) as
*
*              A := A - U(k)*D(k)*U(k)**T = A - W(k)*1/D(k)*W(k)**T
*
               R1 = ONE / AP( KC+K-1 )
               CALL SSPR( UPLO, K-1, -R1, AP( KC ), 1, AP )
*
*              Store U(k) in column k
*
               CALL SSCAL( K-1, R1, AP( KC ), 1 )
            ELSE
*
*              2-by-2 pivot block D(k): columns k and k-1 now hold
*
*              ( W(k-1) W(k) ) = ( U(k-1) U(k) )*D(k)
*
*              where U(k) and U(k-1) are the k-th and (k-1)-th columns
*              of U
*
*              Perform a rank-2 update of A(1:k-2,1:k-2) as
*
*              A := A - ( U(k-1) U(k) )*D(k)*( U(k-1) U(k) )**T
*                 = A - ( W(k-1) W(k) )*inv(D(k))*( W(k-1) W(k) )**T
*
               IF( K.GT.2 ) THEN
*
                  D12 = AP( K-1+( K-1 )*K / 2 )
                  D22 = AP( K-1+( K-2 )*( K-1 ) / 2 ) / D12
                  D11 = AP( K+( K-1 )*K / 2 ) / D12
                  T = ONE / ( D11*D22-ONE )
                  D12 = T / D12
*
                  DO 50 J = K - 2, 1, -1
                     WKM1 = D12*( D11*AP( J+( K-2 )*( K-1 ) / 2 )-
     $                      AP( J+( K-1 )*K / 2 ) )
                     WK = D12*( D22*AP( J+( K-1 )*K / 2 )-
     $                    AP( J+( K-2 )*( K-1 ) / 2 ) )
                     DO 40 I = J, 1, -1
                        AP( I+( J-1 )*J / 2 ) = AP( I+( J-1 )*J / 2 ) -
     $                     AP( I+( K-1 )*K / 2 )*WK -
     $                     AP( I+( K-2 )*( K-1 ) / 2 )*WKM1
   40                CONTINUE
                     AP( J+( K-1 )*K / 2 ) = WK
                     AP( J+( K-2 )*( K-1 ) / 2 ) = WKM1
   50             CONTINUE
*
               END IF
*
            END IF
         END IF
*
*        Store details of the interchanges in IPIV
*
         IF( KSTEP.EQ.1 ) THEN
            IPIV( K ) = KP
         ELSE
            IPIV( K ) = -KP
            IPIV( K-1 ) = -KP
         END IF
*
*        Decrease K and return to the start of the main loop
*
         K = K - KSTEP
         KC = KNC - K
         GO TO 10
*
      ELSE
*
*        Factorize A as L*D*L**T using the lower triangle of A
*
*        K is the main loop index, increasing from 1 to N in steps of
*        1 or 2
*
         K = 1
         KC = 1
         NPP = N*( N+1 ) / 2
   60    CONTINUE
         KNC = KC
*
*        If K > N, exit from loop
*
         IF( K.GT.N )
     $      GO TO 110
         KSTEP = 1
*
*        Determine rows and columns to be interchanged and whether
*        a 1-by-1 or 2-by-2 pivot block will be used
*
         ABSAKK = ABS( AP( KC ) )
*
*        IMAX is the row-index of the largest off-diagonal element in
*        column K, and COLMAX is its absolute value
*
         IF( K.LT.N ) THEN
            IMAX = K + ISAMAX( N-K, AP( KC+1 ), 1 )
            COLMAX = ABS( AP( KC+IMAX-K ) )
         ELSE
            COLMAX = ZERO
         END IF
*
         IF( MAX( ABSAKK, COLMAX ).EQ.ZERO ) THEN
*
*           Column K is zero: set INFO and continue
*
            IF( INFO.EQ.0 )
     $         INFO = K
            KP = K
         ELSE
            IF( ABSAKK.GE.ALPHA*COLMAX ) THEN
*
*              no interchange, use 1-by-1 pivot block
*
               KP = K
            ELSE
*
*              JMAX is the column-index of the largest off-diagonal
*              element in row IMAX, and ROWMAX is its absolute value
*
               ROWMAX = ZERO
               KX = KC + IMAX - K
               DO 70 J = K, IMAX - 1
                  IF( ABS( AP( KX ) ).GT.ROWMAX ) THEN
                     ROWMAX = ABS( AP( KX ) )
                     JMAX = J
                  END IF
                  KX = KX + N - J
   70          CONTINUE
               KPC = NPP - ( N-IMAX+1 )*( N-IMAX+2 ) / 2 + 1
               IF( IMAX.LT.N ) THEN
                  JMAX = IMAX + ISAMAX( N-IMAX, AP( KPC+1 ), 1 )
                  ROWMAX = MAX( ROWMAX, ABS( AP( KPC+JMAX-IMAX ) ) )
               END IF
*
               IF( ABSAKK.GE.ALPHA*COLMAX*( COLMAX / ROWMAX ) ) THEN
*
*                 no interchange, use 1-by-1 pivot block
*
                  KP = K
               ELSE IF( ABS( AP( KPC ) ).GE.ALPHA*ROWMAX ) THEN
*
*                 interchange rows and columns K and IMAX, use 1-by-1
*                 pivot block
*
                  KP = IMAX
               ELSE
*
*                 interchange rows and columns K+1 and IMAX, use 2-by-2
*                 pivot block
*
                  KP = IMAX
                  KSTEP = 2
               END IF
            END IF
*
            KK = K + KSTEP - 1
            IF( KSTEP.EQ.2 )
     $         KNC = KNC + N - K + 1
            IF( KP.NE.KK ) THEN
*
*              Interchange rows and columns KK and KP in the trailing
*              submatrix A(k:n,k:n)
*
               IF( KP.LT.N )
     $            CALL SSWAP( N-KP, AP( KNC+KP-KK+1 ), 1, AP( KPC+1 ),
     $                        1 )
               KX = KNC + KP - KK
               DO 80 J = KK + 1, KP - 1
                  KX = KX + N - J + 1
                  T = AP( KNC+J-KK )
                  AP( KNC+J-KK ) = AP( KX )
                  AP( KX ) = T
   80          CONTINUE
               T = AP( KNC )
               AP( KNC ) = AP( KPC )
               AP( KPC ) = T
               IF( KSTEP.EQ.2 ) THEN
                  T = AP( KC+1 )
                  AP( KC+1 ) = AP( KC+KP-K )
                  AP( KC+KP-K ) = T
               END IF
            END IF
*
*           Update the trailing submatrix
*
            IF( KSTEP.EQ.1 ) THEN
*
*              1-by-1 pivot block D(k): column k now holds
*
*              W(k) = L(k)*D(k)
*
*              where L(k) is the k-th column of L
*
               IF( K.LT.N ) THEN
*
*                 Perform a rank-1 update of A(k+1:n,k+1:n) as
*
*                 A := A - L(k)*D(k)*L(k)**T = A - W(k)*(1/D(k))*W(k)**T
*
                  R1 = ONE / AP( KC )
                  CALL SSPR( UPLO, N-K, -R1, AP( KC+1 ), 1,
     $                       AP( KC+N-K+1 ) )
*
*                 Store L(k) in column K
*
                  CALL SSCAL( N-K, R1, AP( KC+1 ), 1 )
               END IF
            ELSE
*
*              2-by-2 pivot block D(k): columns K and K+1 now hold
*
*              ( W(k) W(k+1) ) = ( L(k) L(k+1) )*D(k)
*
*              where L(k) and L(k+1) are the k-th and (k+1)-th columns
*              of L
*
               IF( K.LT.N-1 ) THEN
*
*                 Perform a rank-2 update of A(k+2:n,k+2:n) as
*
*                 A := A - ( L(k) L(k+1) )*D(k)*( L(k) L(k+1) )**T
*                    = A - ( W(k) W(k+1) )*inv(D(k))*( W(k) W(k+1) )**T
*
*                 where L(k) and L(k+1) are the k-th and (k+1)-th
*                 columns of L
*
                  D21 = AP( K+1+( K-1 )*( 2*N-K ) / 2 )
                  D11 = AP( K+1+K*( 2*N-K-1 ) / 2 ) / D21
                  D22 = AP( K+( K-1 )*( 2*N-K ) / 2 ) / D21
                  T = ONE / ( D11*D22-ONE )
                  D21 = T / D21
*
                  DO 100 J = K + 2, N
                     WK = D21*( D11*AP( J+( K-1 )*( 2*N-K ) / 2 )-
     $                    AP( J+K*( 2*N-K-1 ) / 2 ) )
                     WKP1 = D21*( D22*AP( J+K*( 2*N-K-1 ) / 2 )-
     $                      AP( J+( K-1 )*( 2*N-K ) / 2 ) )
*
                     DO 90 I = J, N
                        AP( I+( J-1 )*( 2*N-J ) / 2 ) = AP( I+( J-1 )*
     $                     ( 2*N-J ) / 2 ) - AP( I+( K-1 )*( 2*N-K ) /
     $                     2 )*WK - AP( I+K*( 2*N-K-1 ) / 2 )*WKP1
   90                CONTINUE
*
                     AP( J+( K-1 )*( 2*N-K ) / 2 ) = WK
                     AP( J+K*( 2*N-K-1 ) / 2 ) = WKP1
*
  100             CONTINUE
               END IF
            END IF
         END IF
*
*        Store details of the interchanges in IPIV
*
         IF( KSTEP.EQ.1 ) THEN
            IPIV( K ) = KP
         ELSE
            IPIV( K ) = -KP
            IPIV( K+1 ) = -KP
         END IF
*
*        Increase K and return to the start of the main loop
*
         K = K + KSTEP
         KC = KNC + N - K + 2
         GO TO 60
*
      END IF
*
  110 CONTINUE
      RETURN
*
*     End of SSPTRF
*
      END
*> \brief \b SSPTRS
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*> \htmlonly
*> Download SSPTRS + dependencies
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.tgz?format=tgz&filename=/lapack/lapack_routine/ssptrs.f">
*> [TGZ]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.zip?format=zip&filename=/lapack/lapack_routine/ssptrs.f">
*> [ZIP]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.txt?format=txt&filename=/lapack/lapack_routine/ssptrs.f">
*> [TXT]</a>
*> \endhtmlonly
*
*  Definition:
*  ===========
*
*       SUBROUTINE SSPTRS( UPLO, N, NRHS, AP, IPIV, B, LDB, INFO )
*
*       .. Scalar Arguments ..
*       CHARACTER          UPLO
*       INTEGER            INFO, LDB, N, NRHS
*       ..
*       .. Array Arguments ..
*       INTEGER            IPIV( * )
*       REAL               AP( * ), B( LDB, * )
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> SSPTRS solves a system of linear equations A*X = B with a real
*> symmetric matrix A stored in packed format using the factorization
*> A = U*D*U**T or A = L*D*L**T computed by SSPTRF.
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] UPLO
*> \verbatim
*>          UPLO is CHARACTER*1
*>          Specifies whether the details of the factorization are stored
*>          as an upper or lower triangular matrix.
*>          = 'U':  Upper triangular, form is A = U*D*U**T;
*>          = 'L':  Lower triangular, form is A = L*D*L**T.
*> \endverbatim
*>
*> \param[in] N
*> \verbatim
*>          N is INTEGER
*>          The order of the matrix A.  N >= 0.
*> \endverbatim
*>
*> \param[in] NRHS
*> \verbatim
*>          NRHS is INTEGER
*>          The number of right hand sides, i.e., the number of columns
*>          of the matrix B.  NRHS >= 0.
*> \endverbatim
*>
*> \param[in] AP
*> \verbatim
*>          AP is REAL array, dimension (N*(N+1)/2)
*>          The block diagonal matrix D and the multipliers used to
*>          obtain the factor U or L as computed by SSPTRF, stored as a
*>          packed triangular matrix.
*> \endverbatim
*>
*> \param[in] IPIV
*> \verbatim
*>          IPIV is INTEGER array, dimension (N)
*>          Details of the interchanges and the block structure of D
*>          as determined by SSPTRF.
*> \endverbatim
*>
*> \param[in,out] B
*> \verbatim
*>          B is REAL array, dimension (LDB,NRHS)
*>          On entry, the right hand side matrix B.
*>          On exit, the solution matrix X.
*> \endverbatim
*>
*> \param[in] LDB
*> \verbatim
*>          LDB is INTEGER
*>          The leading dimension of the array B.  LDB >= max(1,N).
*> \endverbatim
*>
*> \param[out] INFO
*> \verbatim
*>          INFO is INTEGER
*>          = 0:  successful exit
*>          < 0: if INFO = -i, the i-th argument had an illegal value
*> \endverbatim
*
*  Authors:
*  ========
*
*> \author Univ. of Tennessee
*> \author Univ. of California Berkeley
*> \author Univ. of Colorado Denver
*> \author NAG Ltd.
*
*> \date December 2016
*
*> \ingroup realOTHERcomputational
*
*  =====================================================================
      SUBROUTINE SSPTRS( UPLO, N, NRHS, AP, IPIV, B, LDB, INFO )
*
*  -- LAPACK computational routine (version 3.7.0) --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     December 2016
*
*     .. Scalar Arguments ..
      CHARACTER          UPLO
      INTEGER            INFO, LDB, N, NRHS
*     ..
*     .. Array Arguments ..
      INTEGER            IPIV( * )
      REAL               AP( * ), B( LDB, * )
*     ..
*
*  =====================================================================
*
*     .. Parameters ..
      REAL               ONE
      PARAMETER          ( ONE = 1.0E+0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            UPPER
      INTEGER            J, K, KC, KP
      REAL               AK, AKM1, AKM1K, BK, BKM1, DENOM
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. External Subroutines ..
      EXTERNAL           SGEMV, SGER, SSCAL, SSWAP, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     ..
*     .. Executable Statements ..
*
      INFO = 0
      UPPER = LSAME( UPLO, 'U' )
      IF( .NOT.UPPER .AND. .NOT.LSAME( UPLO, 'L' ) ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( NRHS.LT.0 ) THEN
         INFO = -3
      ELSE IF( LDB.LT.MAX( 1, N ) ) THEN
         INFO = -7
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'SSPTRS', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( N.EQ.0 .OR. NRHS.EQ.0 )
     $   RETURN
*
      IF( UPPER ) THEN
*
*        Solve A*X = B, where A = U*D*U**T.
*
*        First solve U*D*X = B, overwriting B with X.
*
*        K is the main loop index, decreasing from N to 1 in steps of
*        1 or 2, depending on the size of the diagonal blocks.
*
         K = N
         KC = N*( N+1 ) / 2 + 1
   10    CONTINUE
*
*        If K < 1, exit from loop.
*
         IF( K.LT.1 )
     $      GO TO 30
*
         KC = KC - K
         IF( IPIV( K ).GT.0 ) THEN
*
*           1 x 1 diagonal block
*
*           Interchange rows K and IPIV(K).
*
            KP = IPIV( K )
            IF( KP.NE.K )
     $         CALL SSWAP( NRHS, B( K, 1 ), LDB, B( KP, 1 ), LDB )
*
*           Multiply by inv(U(K)), where U(K) is the transformation
*           stored in column K of A.
*
            CALL SGER( K-1, NRHS, -ONE, AP( KC ), 1, B( K, 1 ), LDB,
     $                 B( 1, 1 ), LDB )
*
*           Multiply by the inverse of the diagonal block.
*
            CALL SSCAL( NRHS, ONE / AP( KC+K-1 ), B( K, 1 ), LDB )
            K = K - 1
         ELSE
*
*           2 x 2 diagonal block
*
*           Interchange rows K-1 and -IPIV(K).
*
            KP = -IPIV( K )
            IF( KP.NE.K-1 )
     $         CALL SSWAP( NRHS, B( K-1, 1 ), LDB, B( KP, 1 ), LDB )
*
*           Multiply by inv(U(K)), where U(K) is the transformation
*           stored in columns K-1 and K of A.
*
            CALL SGER( K-2, NRHS, -ONE, AP( KC ), 1, B( K, 1 ), LDB,
     $                 B( 1, 1 ), LDB )
            CALL SGER( K-2, NRHS, -ONE, AP( KC-( K-1 ) ), 1,
     $                 B( K-1, 1 ), LDB, B( 1, 1 ), LDB )
*
*           Multiply by the inverse of the diagonal block.
*
            AKM1K = AP( KC+K-2 )
            AKM1 = AP( KC-1 ) / AKM1K
            AK = AP( KC+K-1 ) / AKM1K
            DENOM = AKM1*AK - ONE
            DO 20 J = 1, NRHS
               BKM1 = B( K-1, J ) / AKM1K
               BK = B( K, J ) / AKM1K
               B( K-1, J ) = ( AK*BKM1-BK ) / DENOM
               B( K, J ) = ( AKM1*BK-BKM1 ) / DENOM
   20       CONTINUE
            KC = KC - K + 1
            K = K - 2
         END IF
*
         GO TO 10
   30    CONTINUE
*
*        Next solve U**T*X = B, overwriting B with X.
*
*        K is the main loop index, increasing from 1 to N in steps of
*        1 or 2, depending on the size of the diagonal blocks.
*
         K = 1
         KC = 1
   40    CONTINUE
*
*        If K > N, exit from loop.
*
         IF( K.GT.N )
     $      GO TO 50
*
         IF( IPIV( K ).GT.0 ) THEN
*
*           1 x 1 diagonal block
*
*           Multiply by inv(U**T(K)), where U(K) is the transformation
*           stored in column K of A.
*
            CALL SGEMV( 'Transpose', K-1, NRHS, -ONE, B, LDB, AP( KC ),
     $                  1, ONE, B( K, 1 ), LDB )
*
*           Interchange rows K and IPIV(K).
*
            KP = IPIV( K )
            IF( KP.NE.K )
     $         CALL SSWAP( NRHS, B( K, 1 ), LDB, B( KP, 1 ), LDB )
            KC = KC + K
            K = K + 1
         ELSE
*
*           2 x 2 diagonal block
*
*           Multiply by inv(U**T(K+1)), where U(K+1) is the transformation
*           stored in columns K and K+1 of A.
*
            CALL SGEMV( 'Transpose', K-1, NRHS, -ONE, B, LDB, AP( KC ),
     $                  1, ONE, B( K, 1 ), LDB )
            CALL SGEMV( 'Transpose', K-1, NRHS, -ONE, B, LDB,
     $                  AP( KC+K ), 1, ONE, B( K+1, 1 ), LDB )
*
*           Interchange rows K and -IPIV(K).
*
            KP = -IPIV( K )
            IF( KP.NE.K )
     $         CALL SSWAP( NRHS, B( K, 1 ), LDB, B( KP, 1 ), LDB )
            KC = KC + 2*K + 1
            K = K + 2
         END IF
*
         GO TO 40
   50    CONTINUE
*
      ELSE
*
*        Solve A*X = B, where A = L*D*L**T.
*
*        First solve L*D*X = B, overwriting B with X.
*
*        K is the main loop index, increasing from 1 to N in steps of
*        1 or 2, depending on the size of the diagonal blocks.
*
         K = 1
         KC = 1
   60    CONTINUE
*
*        If K > N, exit from loop.
*
         IF( K.GT.N )
     $      GO TO 80
*
         IF( IPIV( K ).GT.0 ) THEN
*
*           1 x 1 diagonal block
*
*           Interchange rows K and IPIV(K).
*
            KP = IPIV( K )
            IF( KP.NE.K )
     $         CALL SSWAP( NRHS, B( K, 1 ), LDB, B( KP, 1 ), LDB )
*
*           Multiply by inv(L(K)), where L(K) is the transformation
*           stored in column K of A.
*
            IF( K.LT.N )
     $         CALL SGER( N-K, NRHS, -ONE, AP( KC+1 ), 1, B( K, 1 ),
     $                    LDB, B( K+1, 1 ), LDB )
*
*           Multiply by the inverse of the diagonal block.
*
            CALL SSCAL( NRHS, ONE / AP( KC ), B( K, 1 ), LDB )
            KC = KC + N - K + 1
            K = K + 1
         ELSE
*
*           2 x 2 diagonal block
*
*           Interchange rows K+1 and -IPIV(K).
*
            KP = -IPIV( K )
            IF( KP.NE.K+1 )
     $         CALL SSWAP( NRHS, B( K+1, 1 ), LDB, B( KP, 1 ), LDB )
*
*           Multiply by inv(L(K)), where L(K) is the transformation
*           stored in columns K and K+1 of A.
*
            IF( K.LT.N-1 ) THEN
               CALL SGER( N-K-1, NRHS, -ONE, AP( KC+2 ), 1, B( K, 1 ),
     $                    LDB, B( K+2, 1 ), LDB )
               CALL SGER( N-K-1, NRHS, -ONE, AP( KC+N-K+2 ), 1,
     $                    B( K+1, 1 ), LDB, B( K+2, 1 ), LDB )
            END IF
*
*           Multiply by the inverse of the diagonal block.
*
            AKM1K = AP( KC+1 )
            AKM1 = AP( KC ) / AKM1K
            AK = AP( KC+N-K+1 ) / AKM1K
            DENOM = AKM1*AK - ONE
            DO 70 J = 1, NRHS
               BKM1 = B( K, J ) / AKM1K
               BK = B( K+1, J ) / AKM1K
               B( K, J ) = ( AK*BKM1-BK ) / DENOM
               B( K+1, J ) = ( AKM1*BK-BKM1 ) / DENOM
   70       CONTINUE
            KC = KC + 2*( N-K ) + 1
            K = K + 2
         END IF
*
         GO TO 60
   80    CONTINUE
*
*        Next solve L**T*X = B, overwriting B with X.
*
*        K is the main loop index, decreasing from N to 1 in steps of
*        1 or 2, depending on the size of the diagonal blocks.
*
         K = N
         KC = N*( N+1 ) / 2 + 1
   90    CONTINUE
*
*        If K < 1, exit from loop.
*
         IF( K.LT.1 )
     $      GO TO 100
*
         KC = KC - ( N-K+1 )
         IF( IPIV( K ).GT.0 ) THEN
*
*           1 x 1 diagonal block
*
*           Multiply by inv(L**T(K)), where L(K) is the transformation
*           stored in column K of A.
*
            IF( K.LT.N )
     $         CALL SGEMV( 'Transpose', N-K, NRHS, -ONE, B( K+1, 1 ),
     $                     LDB, AP( KC+1 ), 1, ONE, B( K, 1 ), LDB )
*
*           Interchange rows K and IPIV(K).
*
            KP = IPIV( K )
            IF( KP.NE.K )
     $         CALL SSWAP( NRHS, B( K, 1 ), LDB, B( KP, 1 ), LDB )
            K = K - 1
         ELSE
*
*           2 x 2 diagonal block
*
*           Multiply by inv(L**T(K-1)), where L(K-1) is the transformation
*           stored in columns K-1 and K of A.
*
            IF( K.LT.N ) THEN
               CALL SGEMV( 'Transpose', N-K, NRHS, -ONE, B( K+1, 1 ),
     $                     LDB, AP( KC+1 ), 1, ONE, B( K, 1 ), LDB )
               CALL SGEMV( 'Transpose', N-K, NRHS, -ONE, B( K+1, 1 ),
     $                     LDB, AP( KC-( N-K ) ), 1, ONE, B( K-1, 1 ),
     $                     LDB )
            END IF
*
*           Interchange rows K and -IPIV(K).
*
            KP = -IPIV( K )
            IF( KP.NE.K )
     $         CALL SSWAP( NRHS, B( K, 1 ), LDB, B( KP, 1 ), LDB )
            KC = KC - ( N-K+2 )
            K = K - 2
         END IF
*
         GO TO 90
  100    CONTINUE
      END IF
*
      RETURN
*
*     End of SSPTRS
*
      END
*> \brief \b SSWAP
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE SSWAP(N,SX,INCX,SY,INCY)
*
*       .. Scalar Arguments ..
*       INTEGER INCX,INCY,N
*       ..
*       .. Array Arguments ..
*       REAL SX(*),SY(*)
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*>    SSWAP interchanges two vectors.
*>    uses unrolled loops for increments equal to 1.
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] N
*> \verbatim
*>          N is INTEGER
*>         number of elements in input vector(s)
*> \endverbatim
*>
*> \param[in,out] SX
*> \verbatim
*>          SX is REAL array, dimension ( 1 + ( N - 1 )*abs( INCX ) )
*> \endverbatim
*>
*> \param[in] INCX
*> \verbatim
*>          INCX is INTEGER
*>         storage spacing between elements of SX
*> \endverbatim
*>
*> \param[in,out] SY
*> \verbatim
*>          SY is REAL array, dimension ( 1 + ( N - 1 )*abs( INCY ) )
*> \endverbatim
*>
*> \param[in] INCY
*> \verbatim
*>          INCY is INTEGER
*>         storage spacing between elements of SY
*> \endverbatim
*
*  Authors:
*  ========
*
*> \author Univ. of Tennessee
*> \author Univ. of California Berkeley
*> \author Univ. of Colorado Denver
*> \author NAG Ltd.
*
*> \date November 2017
*
*> \ingroup single_blas_level1
*
*> \par Further Details:
*  =====================
*>
*> \verbatim
*>
*>     jack dongarra, linpack, 3/11/78.
*>     modified 12/3/93, array(1) declarations changed to array(*)
*> \endverbatim
*>
*  =====================================================================
      SUBROUTINE SSWAP(N,SX,INCX,SY,INCY)
*
*  -- Reference BLAS level1 routine (version 3.8.0) --
*  -- Reference BLAS is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     November 2017
*
*     .. Scalar Arguments ..
      INTEGER INCX,INCY,N
*     ..
*     .. Array Arguments ..
      REAL SX(*),SY(*)
*     ..
*
*  =====================================================================
*
*     .. Local Scalars ..
      REAL STEMP
      INTEGER I,IX,IY,M,MP1
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC MOD
*     ..
      IF (N.LE.0) RETURN
      IF (INCX.EQ.1 .AND. INCY.EQ.1) THEN
*
*       code for both increments equal to 1
*
*
*       clean-up loop
*
         M = MOD(N,3)
         IF (M.NE.0) THEN
            DO I = 1,M
               STEMP = SX(I)
               SX(I) = SY(I)
               SY(I) = STEMP
            END DO
            IF (N.LT.3) RETURN
         END IF
         MP1 = M + 1
         DO I = MP1,N,3
            STEMP = SX(I)
            SX(I) = SY(I)
            SY(I) = STEMP
            STEMP = SX(I+1)
            SX(I+1) = SY(I+1)
            SY(I+1) = STEMP
            STEMP = SX(I+2)
            SX(I+2) = SY(I+2)
            SY(I+2) = STEMP
         END DO
      ELSE
*
*       code for unequal increments or equal increments not equal
*         to 1
*
         IX = 1
         IY = 1
         IF (INCX.LT.0) IX = (-N+1)*INCX + 1
         IF (INCY.LT.0) IY = (-N+1)*INCY + 1
         DO I = 1,N
            STEMP = SX(IX)
            SX(IX) = SY(IY)
            SY(IY) = STEMP
            IX = IX + INCX
            IY = IY + INCY
         END DO
      END IF
      RETURN
      END
*> \brief \b XERBLA
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE XERBLA( SRNAME, INFO )
*
*       .. Scalar Arguments ..
*       CHARACTER*(*)      SRNAME
*       INTEGER            INFO
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> XERBLA  is an error handler for the LAPACK routines.
*> It is called by an LAPACK routine if an input parameter has an
*> invalid value.  A message is printed and execution stops.
*>
*> Installers may consider modifying the STOP statement in order to
*> call system-specific exception-handling facilities.
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] SRNAME
*> \verbatim
*>          SRNAME is CHARACTER*(*)
*>          The name of the routine which called XERBLA.
*> \endverbatim
*>
*> \param[in] INFO
*> \verbatim
*>          INFO is INTEGER
*>          The position of the invalid parameter in the parameter list
*>          of the calling routine.
*> \endverbatim
*
*  Authors:
*  ========
*
*> \author Univ. of Tennessee
*> \author Univ. of California Berkeley
*> \author Univ. of Colorado Denver
*> \author NAG Ltd.
*
*> \date December 2016
*
*> \ingroup aux_blas
*
*  =====================================================================
      SUBROUTINE XERBLA( SRNAME, INFO )
*
*  -- Reference BLAS level1 routine (version 3.7.0) --
*  -- Reference BLAS is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     December 2016
*
*     .. Scalar Arguments ..
      CHARACTER*(*)      SRNAME
      INTEGER            INFO
*     ..
*
* =====================================================================
*
*     .. Intrinsic Functions ..
      INTRINSIC          LEN_TRIM
*     ..
*     .. Executable Statements ..
*
      WRITE( *, FMT = 9999 )SRNAME( 1:LEN_TRIM( SRNAME ) ), INFO
*
      STOP
*
 9999 FORMAT( ' ** On entry to ', A, ' parameter number ', I2, ' had ',
     $      'an illegal value' )
*
*     End of XERBLA
*
      END