program test_demos
  implicit none
  integer, parameter :: dp = selected_real_kind(15,307)
  real(dp) :: a,b
  integer :: n
  real(dp) :: trap, simp, g2, g3
  ! derivative tests
  real(dp) :: x,h
  real(dp) :: d_central, d_complex, d_rich
  ! RK4 test
  real(dp) :: y, t, h_rk
  integer :: steps
  ! GBM test
  real(dp) :: mean_num, mean_analytic

  ! Integral tests for f(x)=x**2 on [0,1]
  a = 0.0_dp; b = 1.0_dp; n = 1000
  trap = trap_int_poly2(a,b,n)
  simp = simpson_int_poly2(a,b,n)
  g2 = gauss2_poly2(a,b)
  g3 = gauss3_poly2(a,b)
  write(*,'(A)') 'Integral tests for x^2 on [0,1], analytic=1/3'
  write(*,'(A,ES18.10)') '  Trapezoid =', trap
  write(*,'(A,ES18.10)') '  Simpson   =', simp
  write(*,'(A,ES18.10)') '  Gauss2    =', g2
  write(*,'(A,ES18.10)') '  Gauss3    =', g3

  ! Derivative tests at x=1 for exp
  x = 1.0_dp; h = 1.0e-5_dp
  d_central = deriv1_exp(x,h)
  d_complex = deriv1_complex_exp(x,1.0e-10_dp)
  d_rich = deriv1_richardson_exp(x,1.0e-3_dp,4)
  write(*,'(A)') 'Derivative tests for exp at x=1, exact=exp(1)'
  write(*,'(A,ES18.10)') '  central   =', d_central
  write(*,'(A,ES18.10)') '  complex   =', d_complex
  write(*,'(A,ES18.10)') '  richardson=', d_rich
  write(*,'(A,ES18.10)') '  exact     =', exp(1.0_dp)

  ! RK4 for dy/dx = -y, y(0)=1 over 1.0 with h=0.1
  y = 1.0_dp; t = 0.0_dp; h_rk = 0.1_dp; steps = 10
  call rk4_decay(t,y,h_rk,steps)

  ! Small GBM mean test
  mean_num = stochastic_gbm_mean(1.0_dp, 0.1_dp, 0.2_dp, 1.0_dp, 100, 1000)
  mean_analytic = 1.0_dp * exp(0.1_dp * 1.0_dp)
  write(*,'(A)') 'GBM mean test:'
  write(*,'(A,ES18.10)') '  numeric mean =', mean_num
  write(*,'(A,ES18.10)') '  analytic mean=', mean_analytic

contains
  ! copy minimal implementations from main file for testing
  real(dp) function trap_int_poly2(a, b, n)
    implicit none
    real(dp), intent(in) :: a, b
    integer, intent(in) :: n
    integer :: i
    real(dp) :: h, xi, sum
    h = (b-a)/n
    sum = 0.0_dp
    do i = 1, n-1
      xi = a + i*h
      sum = sum + xi**2
    end do
    trap_int_poly2 = h * (0.5_dp*(a**2 + b**2) + sum)
  end function trap_int_poly2

  real(dp) function simpson_int_poly2(a, b, n)
    implicit none
    real(dp), intent(in) :: a, b
    integer, intent(in) :: n
    integer :: i, nm
    real(dp) :: h, xi, sum
    nm = n
    if (mod(nm,2) /= 0) then
      nm = nm + 1
    end if
    h = (b-a)/real(nm,dp)
    sum = 0.0_dp
    do i = 1, nm-1
      xi = a + i*h
      if (mod(i,2) == 0) then
        sum = sum + 2.0_dp*xi**2
      else
        sum = sum + 4.0_dp*xi**2
      end if
    end do
    simpson_int_poly2 = (h/3.0_dp) * (a**2 + b**2 + sum)
  end function simpson_int_poly2

  real(dp) function gauss2_poly2(a,b)
    implicit none
    real(dp), intent(in) :: a, b
    real(dp) :: c,m,t1,t2,x1,x2
    c = 0.5*(b+a); m=0.5*(b-a)
    t1 = -1.0_dp/sqrt(3.0_dp); t2 = -t1
    x1 = c + m*t1; x2 = c + m*t2
    gauss2_poly2 = m*(x1**2 + x2**2)
  end function gauss2_poly2

  real(dp) function gauss3_poly2(a,b)
    implicit none
    real(dp), intent(in) :: a,b
    real(dp) :: c,m,t1,t2,t3,w1,w2,w3,x1,x2,x3
    c = 0.5*(b+a); m=0.5*(b-a)
    t1 = -sqrt(3.0_dp/5.0_dp); t2 = 0.0_dp; t3 = -t1
    w1 = 5.0_dp/9.0_dp; w2 = 8.0_dp/9.0_dp; w3 = w1
    x1 = c + m*t1; x2 = c + m*t2; x3 = c + m*t3
    gauss3_poly2 = m*(w1*x1**2 + w2*x2**2 + w3*x3**2)
  end function gauss3_poly2

  real(dp) function deriv1_exp(x,h)
    implicit none
    real(dp), intent(in) :: x,h
    deriv1_exp = (exp(x+h) - exp(x-h)) / (2.0_dp*h)
  end function deriv1_exp

  real(dp) function deriv1_complex_exp(x,h)
    implicit none
    real(dp), intent(in) :: x,h
    complex(dp) :: z
    z = cmplx(x,h,kind=dp)
    deriv1_complex_exp = aimag(exp(z)) / h
  end function deriv1_complex_exp

  real(dp) function deriv1_richardson_exp(x,h,levels)
    implicit none
    real(dp), intent(in) :: x,h
    integer, intent(in) :: levels
    integer :: k,j
    real(dp) :: hk
    real(dp), allocatable :: D(:)
    real(dp), allocatable :: R(:,:)
    allocate(D(0:levels-1))
    allocate(R(0:levels-1,0:levels-1))
    do k = 0, levels-1
      hk = h / (2.0_dp**k)
      D(k) = (exp(x+hk) - exp(x-hk)) / (2.0_dp*hk)
      R(k,0) = D(k)
    end do
    do j = 1, levels-1
      do k = j, levels-1
        R(k,j) = R(k,j-1) + (R(k,j-1) - R(k-1,j-1)) / (4.0_dp**j - 1.0_dp)
      end do
    end do
    deriv1_richardson_exp = R(levels-1,levels-1)
    deallocate(D); deallocate(R)
  end function deriv1_richardson_exp

  subroutine rk4_decay(x0,y0,h,steps)
    implicit none
    real(dp), intent(in) :: x0,y0,h
    integer, intent(in) :: steps
    integer :: i
    real(dp) :: x,y,k1,k2,k3,k4
    x = x0; y = y0
    write(*,'(A)') 'RK4 progression (dy/dx=-y):'
    do i = 1, steps
      k1 = -y
      k2 = -(y + 0.5*h*k1)
      k3 = -(y + 0.5*h*k2)
      k4 = -(y + h*k3)
      y = y + (h/6.0_dp)*(k1 + 2.0_dp*k2 + 2.0_dp*k3 + k4)
      x = x + h
      write(*,'(A,F8.4,A,ES18.10)') '  x=', x, '  y=', y
    end do
  end subroutine rk4_decay

  ! Simple normal RNG (Box-Muller)
  real(dp) function normal_rand()
    implicit none
    real(dp) :: u1,u2
    call random_number(u1); call random_number(u2)
    if (u1 < 1.0e-30_dp) u1 = 1.0e-30_dp
    normal_rand = sqrt(-2.0_dp*log(u1)) * cos(2.0_dp*3.141592653589793_dp*u2)
  end function normal_rand

  real(dp) function stochastic_gbm_mean(x0, mu, sigma, T, Nsteps, Msamp)
    implicit none
    real(dp), intent(in) :: x0, mu, sigma, T
    integer, intent(in) :: Nsteps, Msamp
    integer :: samp, step
    real(dp) :: dt_local, xpath, z
    dt_local = T / real(Nsteps,dp)
    stochastic_gbm_mean = 0.0_dp
    do samp = 1, Msamp
      xpath = x0
      do step = 1, Nsteps
        z = normal_rand()
        xpath = xpath + mu*xpath*dt_local + sigma*xpath*sqrt(dt_local)*z
      end do
      stochastic_gbm_mean = stochastic_gbm_mean + xpath
    end do
    stochastic_gbm_mean = stochastic_gbm_mean / real(Msamp,dp)
  end function stochastic_gbm_mean

end program test_demos
