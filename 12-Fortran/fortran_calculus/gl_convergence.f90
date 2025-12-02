program gl_convergence
  implicit none
  integer, parameter :: dp = selected_real_kind(15,307)
  real(dp), parameter :: x0 = 1.0_dp
  real(dp), parameter :: alpha = 0.5_dp
  real(dp) :: analytic
  real(dp), allocatable :: hvals(:)
  integer, allocatable :: mults(:)
  integer :: ih, im, N
  real(dp) :: h, num, relerr
  integer :: i

  analytic = 2.0_dp / gamma(3.0_dp - alpha) * x0**(2.0_dp - alpha)

  hvals = (/ 1.0e-1_dp, 1.0e-2_dp, 1.0e-3_dp, 1.0e-4_dp, 1.0e-5_dp /)
  mults = (/ 1, 2, 5, 10, 20 /)

  write(*,'(A)') 'GL convergence study for f(x)=x^2, x=1, alpha=0.5'
  write(*,'(A)') 'Columns: h, mult, N, numeric, analytic, rel_error'
  do ih = 1, size(hvals)
    h = hvals(ih)
    do im = 1, size(mults)
      ! choose N so that history covers x0 with multiplier
      N = max(10, int( ceiling( x0 / h ) ) * mults(im) )
      if (N > 200000) N = 200000
      ! Use adaptive, Kahan-summed GL implementation with optional Richardson
      num = frac_deriv_gl_poly2_adaptive(x0, alpha, h, N, 1.0e-12_dp)
      relerr = abs(num - analytic) / max(1.0e-30_dp, abs(analytic))
      write(*,'(ES12.5,2X,I5,2X,I6,2X,ES18.10,2X,ES18.10,2X,ES18.10)') h, mults(im), N, num, analytic, relerr
    end do
  end do
contains
  ! Adaptive Grünwald–Letnikov fractional derivative with Kahan summation
  real(dp) function frac_deriv_gl_poly2_adaptive(x, alpha, h, Nmax, tol)
    implicit none
    real(dp), intent(in) :: x, alpha, h, tol
    integer, intent(in) :: Nmax
    integer :: k, N
    real(dp) :: coeff, term, arg, sum, c, abs_term
    ! Kahan summation variables
    sum = 0.0_dp
    c = 0.0_dp
    coeff = 1.0_dp
    do k = 0, Nmax
      if (k > 0) then
        coeff = coeff * (alpha - real(k-1,dp)) / real(k,dp)
      end if
      arg = x - real(k,dp)*h
      term = (-1.0_dp)**k * coeff * (arg**2)
      ! Kahan sum: y = term - c; t = sum + y; c = (t - sum) - y; sum = t
      term = term - c
      sum = sum + term
      c = (sum - (sum - term)) - term
      abs_term = abs(term)
      if (abs_term < tol) then
        N = k
        exit
      end if
    end do
    if (k >= Nmax) N = Nmax
    frac_deriv_gl_poly2_adaptive = sum / (h**alpha)
  end function frac_deriv_gl_poly2_adaptive
end program gl_convergence
