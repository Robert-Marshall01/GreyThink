program test_frac
  implicit none
  integer, parameter :: dp = selected_real_kind(15,307)
  real(dp) :: num, analytic, eps, delta

  num = frac_deriv_gl_poly2(1.0_dp, 0.5_dp, 1.0e-3_dp, 200)
  analytic = 2.0_dp / gamma(3.0_dp - 0.5_dp)
  write(*,'(A,ES18.10)') 'numeric    =', num
  write(*,'(A,ES18.10)') 'analytic   =', analytic
  write(*,'(A,ES18.10)') 'rel_error  =', abs(num-analytic)/max(1.0e-30_dp,abs(analytic))

  eps = 1.0e-6_dp
  delta = find_delta_sin_over_x(eps)
  write(*,'(A,F12.6)') 'delta for eps=', eps
  write(*,'(A,F12.6)') 'returned delta=', delta

contains
  real(dp) function frac_deriv_gl_poly2(x, alpha, h, N)
    implicit none
    real(dp), intent(in) :: x, alpha, h
    integer, intent(in) :: N
    integer :: k
    real(dp) :: sum, coeff, arg
    sum = 0.0_dp
    coeff = 1.0_dp
    do k = 0, N
      if (k > 0) then
        coeff = coeff * (alpha - real(k-1,dp)) / real(k,dp)
      end if
      arg = x - k*h
      sum = sum + (-1.0_dp)**k * coeff * (arg**2)
    end do
    frac_deriv_gl_poly2 = sum / (h**alpha)
  end function frac_deriv_gl_poly2

  real(dp) function find_delta_sin_over_x(eps)
    implicit none
    real(dp), intent(in) :: eps
    real(dp) :: delta, x, val
    integer :: i, samples
    logical :: success
    samples = 12
    find_delta_sin_over_x = -1.0_dp
    delta = 1.0e-8_dp
    do while (delta <= 1.0e-1_dp)
      success = .true.
      do i = 1, samples
        x = delta * real(i,dp) / real(samples,dp)
        val = abs( sin(x)/x - 1.0_dp )
        if (val >= eps) then
          success = .false.
          exit
        end if
      end do
      if (success) then
        find_delta_sin_over_x = delta
        return
      end if
      delta = delta * 2.0_dp
    end do
  end function find_delta_sin_over_x

end program test_frac
