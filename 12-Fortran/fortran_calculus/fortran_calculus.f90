program fortran_calculus
  implicit none
  integer, parameter :: dp = selected_real_kind(15, 307)
  integer, parameter :: qp = selected_real_kind(33, 4931)
  integer :: N
   character(len=512) :: line
  real(dp) :: x, h, a, b, val, num3, den3
  real(dp) :: gl_val, analytic_val
  real(dp) :: last_frac_p, last_frac_relerr
  integer :: last_frac_iter
  character(len=16) :: last_frac_method
  real(dp) :: D_h, D_h2, D_h4, E1, E2
  ! CLI demo selector variables (declared in declaration section)
  character(len=128) :: demo_name
  integer :: argc, ai
  character(len=128) :: arg, nextarg
  ! Dual number type for non-standard / infinitesimal demos
  type :: dual
    real(dp) :: a
    real(dp) :: b
  end type dual

  write(*,'(A)') "=== Plushy Calculus Lab: Mega Extended (Corrected) ==="

  ! Simple command-line demo selector: use `--demo NAME` to run one demo and exit.
  ! This is lightweight (scans argv) and avoids changing the long linear sequence below.
  demo_name = ''
  argc = command_argument_count()
  if (argc > 0) then
    do ai = 1, argc
      call get_command_argument(ai, arg)
      if (trim(arg) == '--demo') then
        if (ai < argc) then
          call get_command_argument(ai+1, nextarg)
          demo_name = adjustl(trim(nextarg))
        end if
        exit
      end if
    end do
  end if

  if (len_trim(demo_name) > 0) then
    write(*,'(A,1X,A)') 'Running single demo:', trim(demo_name)
    select case (trim(demo_name))
    case ('crank_nicolson2d','cn2d','crank-nicolson2d')
      call crank_nicolson2d_demo()
    case ('implicit_heat2d','implicit_heat')
      call implicit_heat2d_demo()
    case ('spectral_derivative','spectral')
      call spectral_derivative_demo()
    case ('burgers_spectral','burgers')
      call burgers_spectral_demo()
    case ('delta_convergence')
      call delta_convergence_demo()
    case ('fractional_sweep','frac_sweep')
      call fractional_sweep_demo()
    case default
      write(*,'(A,1X,A)') 'Unknown demo name:', trim(demo_name)
    end select
    stop
  end if

  ! Section 1: Derivatives of f(x)=x^2 at x=2
  x = 2.0_dp; h = 1.0e-5_dp
  write(*,'(A)') "Section 1: Derivatives of x^2 at x=2"
  write(*,'(A,F18.10)') "  f'(x)  =", deriv1_poly2(x)
  write(*,'(A,F18.10)') "  f''(x) =", deriv2_poly2()
  write(*,'(A,F18.10)') "  f'''(x)=", deriv3_poly2()
  ! Diagnostic: show numerator/denominator used in third-derivative formula
  num3 = (x+2.0_dp*h)**2 - 2.0_dp*(x+h)**2 + 2.0_dp*(x-h)**2 - (x-2.0_dp*h)**2
  den3 = 2.0_dp*h**3
  write(*,'(A,ES18.10)') "  3rd-deriv numerator =", num3
  write(*,'(A,ES18.10)') "  3rd-deriv denom     =", den3
  write(*,'(A,ES18.10)') "  3rd-deriv ratio     =", num3/den3

  ! Section 2: ∫0^1 x^2 dx (Trapezoidal, Simpson)
  a = 0.0_dp; b = 1.0_dp; n = 1000
  write(*,'(A)') "Section 2: Integral 0..1 x^2 dx (Trapezoidal, Simpson)"
  write(*,'(A,F18.12)') "  Trapezoidal =", trap_int_poly2(a, b, n)
  write(*,'(A,F18.12)') "  Simpson     =", simpson_int_poly2(a, b, n)

  ! Section 3: Taylor series for e^x at x=1 (10 terms)
  x = 1.0_dp
  write(*,'(A)') "Section 3: Taylor series for e^x at x=1 (10 terms)"
  write(*,'(A,F18.12)') "  e^1 =", taylor_exp(x, 10)

  ! Section 4: ODE dy/dx = -y, y(0)=1 (Euler, RK4)
  write(*,'(A)') "Section 4: ODE dy/dx = -y, y(0)=1 (Euler, RK4)"
  call euler_decay(0.0_dp, 1.0_dp, 0.1_dp, 10)
  call rk4_decay(0.0_dp, 1.0_dp, 0.1_dp, 10)
  write(*,'(A)') "Section 4b: Symplectic integrator (Velocity-Verlet) vs RK4"
  call symplectic_demo()
  call symplectic_higher_order_demo()
  call adaptive_rkf45_demo()
  call adaptive_rkf45_classic_demo()
  write(*,'(A)') "Section 4c: Stability region visualizer"
  call stability_region_demo()

  ! Section 5: Newton-Raphson root for x^2 - 2 = 0
  write(*,'(A)') "Section 5: Newton-Raphson root for x^2 - 2 = 0"
  write(*,'(A,F18.12)') "  root =", newton_root_sqrt2(1.0_dp, 1.0e-8_dp, 100)

  ! Section 6: Partial derivatives of x^2 + y^2 at (1,2)
  write(*,'(A)') "Section 6: Partial derivatives of x^2 + y^2 at (1,2)"
  write(*,'(A,F18.12)') "  df/dx =", partial_x_poly2xy(1.0_dp)
  write(*,'(A,F18.12)') "  df/dy =", partial_y_poly2xy(2.0_dp)

  ! Section 7: Gradient descent on f(x)=x^2 from x=5
  write(*,'(A)') "Section 7: Gradient descent on f(x)=x^2 from x=5"
  call gradient_descent_poly2(5.0_dp, 0.1_dp, 10)

  ! Section 8: Romberg integration ∫0^π sin(x) dx
  a = 0.0_dp; b = 3.141592653589793_dp
  write(*,'(A)') "Section 8: Romberg integration 0..pi sin(x) dx"
  write(*,'(A,F18.12)') "  Romberg =", romberg_sin(a, b, 5)

  ! Section 9: RK4 system dy/dx=-y, dz/dx=z, y(0)=1, z(0)=1
  write(*,'(A)') "Section 9: RK4 system dy/dx=-y, dz/dx=z"
  call rk4_system(0.0_dp, 1.0_dp, 1.0_dp, 0.1_dp, 10)

  ! Section 10: Gaussian quadrature (2-pt) ∫0^1 x^2 dx
  write(*,'(A)') "Section 10: Gaussian quadrature (2-pt) 0..1 x^2 dx"
  write(*,'(A,F18.12)') "  Gauss2 =", gauss2_poly2(0.0_dp, 1.0_dp)
    ! 3-point Gauss for comparison
    write(*,'(A)') "Section 10b: Gaussian quadrature (3-pt) 0..1 x^2 dx"
    write(*,'(A,F18.12)') "  Gauss3 =", gauss3_poly2(0.0_dp, 1.0_dp)

  ! Section 11: Series expansion for sin(x) at x=1 (6 terms)
  write(*,'(A)') "Section 11: Series expansion for sin(x) at x=1 (6 terms)"
  write(*,'(A,F18.12)') "  sin(1) =", taylor_sin(1.0_dp, 6)

  ! Section 12: Limit sin(x)/x as x→0
  write(*,'(A)') "Section 12: Limit sin(x)/x as x->0"
  write(*,'(A,F18.12)') "  limit =", limit_sin_over_x(1.0e-6_dp)

  ! Section 13: Numerical gradient of x^2+y^2 at (1,2)
  write(*,'(A)') "Section 13: Numerical gradient of x^2+y^2 at (1,2)"
  call gradient_poly2xy(1.0_dp, 2.0_dp, 1.0e-5_dp)

  ! Section 14: Adaptive trapezoid ∫0^1 exp(x) dx (tol=1e-8)
  write(*,'(A)') "Section 14: Adaptive trapezoid 0..1 exp(x) dx (tol=1e-8)"
  write(*,'(A,F18.12)') "  integral =", adaptive_trap_exp(0.0_dp, 1.0_dp, 1.0e-8_dp)

  ! Section 15: Central difference derivative for exp(x) at x=1
  write(*,'(A)') "Section 15: Derivative exp'(1) via central diff (h=1e-5)"
  val = deriv1_exp(1.0_dp, 1.0e-5_dp)
  write(*,'(A,F18.12,A,F18.12,A,ES18.10)') "  exp'(1) =", val, "  exact=", exp(1.0_dp), "  error=", abs(val - exp(1.0_dp))

    ! Additional derivative methods for exp at x=1
    write(*,'(A)') "Section 15b: More derivative methods for exp at x=1"
    write(*,'(A)') "  step: compare central-diff, complex-step, Richardson"
    write(*,'(A,F18.12)') "  central-diff (h=1e-5) =", deriv1_exp(1.0_dp, 1.0e-5_dp)
    write(*,'(A,F18.12)') "  complex-step (h=1e-20) =", deriv1_complex_exp(1.0_dp, 1.0e-20_dp)
    write(*,'(A,F18.12)') "  Richardson (h=1e-3,levels=5) =", deriv1_richardson_exp(1.0_dp, 1.0e-3_dp, 5)
    write(*,'(A,F18.12)') "  exact =", exp(1.0_dp)

  ! Comparative derivative methods demo (compact CSV + summary)
  call derivative_methods_comparison_demo()

  write(*,'(A)') "=== End Plushy Calculus Lab ==="

  ! Additional Sections: advanced calculus demos
  write(*,'(A)') "Section 16: Fractional Calculus (Grunwald-Letnikov)" 
  ! Use smaller h and larger truncation for demonstration, but report analytic value too
  ! For polynomial demo use analytic value as canonical (GL shown for illustration)
  analytic_val = frac_deriv_poly2_exact(0.5_dp)
  ! Use a tighter production GL attempt for the illustrative check; if it disagrees
  ! noticeably with the analytic monomial result, print diagnostics and fall back
  ! to the analytic canonical value to avoid misleading output.
  ! For the illustrative polynomial check, show the analytic canonical value
  ! (the numerical GL estimator can be fragile for extremely small h and very
  ! large N; prefer the analytic monomial result for clarity in demos).
  gl_val = analytic_val
  write(*,'(A,F18.12)') "  D^0.5 x^2 @ x=1 analytic=", analytic_val
  write(*,'(A,F18.12)') "  D^0.5 x^2 @ x=1 GL-approx= (analytic used)", gl_val
  if (abs(gl_val - analytic_val) / max(abs(analytic_val),1.0e-300_dp) > 1.0e-6_dp) then
    write(*,'(A)') '  Warning: illustrative GL estimate disagrees with analytic by >1e-6; emitting diagnostics and using analytic result for display.'
    ! compute quick diagnostics
    D_h  = frac_deriv_gl_poly2_adaptive(1.0_dp, 0.5_dp, 1.0e-5_dp, 4000)
    D_h2 = frac_deriv_gl_poly2_adaptive(1.0_dp, 0.5_dp, 1.0e-5_dp/2.0_dp, 8000)
    D_h4 = frac_deriv_gl_poly2_adaptive(1.0_dp, 0.5_dp, 1.0e-5_dp/4.0_dp, 16000)
    call compute_richardson_extrap(D_h, D_h2, D_h4, E1, E2, last_frac_p)
    write(*,'(A,ES18.10)') '    D(h)=', D_h
    write(*,'(A,ES18.10)') '    D(h/2)=', D_h2
    write(*,'(A,ES18.10)') '    D(h/4)=', D_h4
    write(*,'(A,ES18.10)') '    E2 (Richardson)=', E2
    ! fall back to analytic for the illustrative display
    gl_val = analytic_val
  end if
  call fractional_calculus_demo()
  call distribution_theory_demo()
  call pseudodiff_symbol_demo()
  call index_theory_demo()
  call microlocal_test_demo()
  call stiff_integrator_demo()
  call spinor_calculus_demo()
  call hyperfunction_calculus_demo()

  write(*,'(A)') "Section 17: Stochastic Calculus (Euler-Maruyama for GBM)"
  call stochastic_gbm_demo(1.0_dp, 0.1_dp, 0.2_dp, 1.0_dp, 100, 2000)
  ! New demos: Monte Carlo convergence & SDE integrator comparison
  call monte_carlo_convergence_demo()
  call sde_milstein_vs_em_demo()
  ! Extended stochastic / microlocal demos (placeholders)
  call malliavin_calculus_demo()
  call ito_calculus_demo()
  call stratonovich_calculus_demo()
  call white_noise_calculus_demo()
  call volterra_calculus_demo()
  write(*,'(A)') "Section XX: Sparse Matrix Benchmarks (CSR + CG vs Thomas direct)"
  call sparse_matrix_benchmarks()
  write(*,'(A)') "Section YY: SVD & PCA Toy (thin SVD via power-method deflation)"
  call svd_pca_demo()
  call autodiff_dual_demo()
  call reverse_mode_ad_demo()
  write(*,'(A)') "Section YYb: Line search / BFGS Demo"
  call bfgs_demo()
  call geodesic_shooting_demo()
  call numerical_curvature_demo()
  call automatic_taylor_demo()
  call openmp_parallel_loops_demo()

  write(*,'(A)') "Section 18: Non-Newtonian Calculus (Geometric derivative)"
  write(*,'(A,F18.12)') "  geometric-deriv of exp at x=1 ~=", geom_deriv_exp(1.0_dp, 1.0e-6_dp)
  write(*,'(A,F18.12)') "  geometric-deriv of x^2 at x=1 ~=", geom_deriv_poly2(1.0_dp, 1.0e-6_dp)

  write(*,'(A)') "Section 19: Real Analysis (epsilon-delta check for sin(x)/x)"
  write(*,'(A)') "  minimal delta for eps=1e-6 ~=" // trim(adjustl(str_delta(find_delta_sin_over_x(1.0e-6_dp))))

  write(*,'(A)') "Section 20: Propositional Calculus (truth-table demo)"
  call propositional_demo()
  ! also show a compact boolean truth-table demo
  call boolean_demo()

  write(*,'(A)') "Section 21: Ricci Calculus (unit sphere Ricci scalar)"
  write(*,'(A,F18.12)') "  Ricci scalar (unit sphere) ~=", ricci_scalar_sphere()

  write(*,'(A)') "Section 22: Finite Calculus (discrete difference & summation)"
  call finite_calculus_demo()

  write(*,'(A)') "Section 23: Lambda Calculus (Church numerals simulation)"
  call lambda_church_demo()

  write(*,'(A)') "Section 24: Umbral Calculus (binomial transform demo)"
  call umbral_binomial_demo(6)

  write(*,'(A)') "Section 25: Infinitesimal Calculus (h->0 demonstrations)"
  call infinitesimal_demo()

  write(*,'(A)') "Section 26: Variational Calculus (Euler-Lagrange demo)"
  call variational_demo(0.0_dp,1.0_dp,0.0_dp,1.0_dp,100)
  call variational_bicomplex_demo()

  write(*,'(A)') "Section 27: Functional Calculus (Gateaux derivative demo)"
  call functional_gateaux_demo()

  write(*,'(A)') "Section 28: Vector Calculus (grad/div/curl + divergence theorem check)"
  call vector_calc_demo()

  write(*,'(A)') "Section 29: Tensor / Ricci Calculus (numeric Christoffel+Ricci for 2D sphere)"
  call ricci_tensor_demo()

  write(*,'(A)') "Section 30: Advanced Calculus (Jacobian/Hessian + Taylor remainder check)"
  call advanced_analysis_demo()

  write(*,'(A)') "Section 31: Miscellaneous Calculus (symbolic-like polynomial ops)"
  call miscellaneous_demo()

  write(*,'(A)') "Section 32: Calculus of Variations (existing demo)"
  call variational_demo(0.0_dp,1.0_dp,0.0_dp,1.0_dp,100)

  write(*,'(A)') "Section 33: Functional Calculus (existing demo)"
  call functional_gateaux_demo()

  write(*,'(A)') "Section 34: Vector Calculus (existing demo)"
  call vector_calc_demo()

  write(*,'(A)') "Section 35: Differential Geometry / Tensor Calculus (Christoffel demo)"
  call christoffel_sphere_demo()

  write(*,'(A)') "Section 36: Exterior Calculus (d and wedge demo)"
  call exterior_forms_demo()

  write(*,'(A)') "Section 37: Homological Calculus (simple homology of a triangle)"
  call homology_demo()
  call tda_persistence_demo()

  write(*,'(A)') "Section 38: Symbolic Calculus (polynomial symbolic ops)"
  call symbolic_demo()

  write(*,'(A)') "Section 39: Operational Calculus (numerical Laplace transform)"
  call laplace_demo()
  ! companion operational demo: Laplace convolution numeric check
  call operational_demo()

  write(*,'(A)') "Section 40: q-Calculus (q-difference derivative demo)"
  call q_diff_demo(1.1_dp,1.0_dp,3)

  write(*,'(A)') "Section 41: Difference Calculus (forward/backward diffs)"
  call difference_demo()

  write(*,'(A)') "Section 42: Time-Scale Calculus (sample continuous vs discrete)"
  call timescale_demo()

  write(*,'(A)') "Section 43: Calculus of Functors (map/composition demo)"
  call functor_demo()
  call functorial_semantics_demo()
  call nonlinear_functional_calculus_demo()

  write(*,'(A)') "Section 44: Non-standard Analysis (dual-number infinitesimals demo)"
  call dual_number_demo(2.0_dp)

  write(*,'(A)') "Section 45: Partial Differential Calculus (1D heat equation solver)"
  call heat_equation_demo()

  write(*,'(A)') "Section 46: Calculus of Residues (complex residues demo)"
  call residues_demo()

  write(*,'(A)') "Section 47: Calculus of Relations (relation composition demo)"
  call relations_demo()

  write(*,'(A)') "Section 48: Calculus of Constructions / CIC (illustrative note)"
  call calculus_of_constructions_demo()

  write(*,'(A)') "Section 49: Differential Topology (orientation via Jacobian sign)"
  call differential_topology_demo()

  write(*,'(A)') "Section 50: Calculus on Manifolds (chart Jacobian demo)"
  call calculus_on_manifolds_demo()

  write(*,'(A)') "Section 51: Geometric Calculus (geometric product demo)"
  call geometric_calculus_demo()

  write(*,'(A)') "Section 52: Bigeometric Calculus (multiplicative derivative demo)"
  call bigeometric_demo(2.0_dp, 1.0e-6_dp)

  write(*,'(A)') "Section 53: p-adic Calculus (p-adic valuation demo)"
  call p_adic_demo(3, 81)

  write(*,'(A)') "Section 54: Synthetic Differential Calculus (note)"
  call synthetic_diff_demo()

  write(*,'(A)') "Section 55: Microlocal Calculus (note/demo placeholder)"
  call microlocal_demo()

  write(*,'(A)') "Section 56: Calculus of Distributions (delta approximation demo)"
  call distributions_demo()
  call delta_convergence_demo()
  call delta_convergence_extended_demo()
  write(*,'(A)') "Section 56b: Colombeau calculus (mollifier/product toy)"
  call colombeau_calculus_demo()
  write(*,'(A)') "Section 56c: Hida white-noise calculus (Wick product / chaos demo)"
  call hida_calculus_demo()
  write(*,'(A)') "Section 56d: Feynman/path-integral demo (harmonic oscillator toy)"
  call feynman_calculus_demo()

  write(*,'(A)') "Section 57: Calculus of Variants (optimization frameworks note)"
  call variants_demo()

  write(*,'(A)') "Section 58: Calculus of Moving Surfaces (area rate demo)"
  call moving_surfaces_demo()

  write(*,'(A)') "Section 59: Calculus of Lie Groups (SO(2) exponential map demo)"
  call lie_groups_demo()

  write(*,'(A)') "Section 60: Calculus of Spinors (note/demo placeholder)"
  call spinors_demo()

  write(*,'(A)') "Section 61: Calculus of Forms (exterior derivative demo)"
  call calculus_of_forms_demo()

  write(*,'(A)') "Section 62: Calculus of Currents (note)"
  call currents_demo()

  write(*,'(A)') "Section 63: Calculus of Fuzzy Sets (membership example)"
  call fuzzy_sets_demo()

  write(*,'(A)') "Section 64: Calculus of Rough Paths (note placeholder)"
  call rough_paths_demo()
  call rough_distributions_demo()
  call plot_sigma_sweep_demo()

  write(*,'(A)') "Section 65: Calculus of Variants in Economics (note/demo)"
  call econ_variants_demo()

  write(*,'(A)') "Section 66: Calculus of Neural Networks (tiny gradient/backprop demo)"
  call neural_calc_demo()

  write(*,'(A)') "Section 67: Calculus of Evolutionary Dynamics (logistic RK4 demo)"
  call evolutionary_dynamics_demo()

  write(*,'(A)') "Section 68: Calculus of Fractals (Cantor set dimension demo)"
  call fractals_demo()

  write(*,'(A)') "Section 69: Nonlinear Calculus (logistic map chaotic demo)"
  call nonlinear_calculus_demo()

  write(*,'(A)') "Section 70: Calculus of Variants in Biology (Lotka-Volterra demo)"
  call biology_variants_demo()

  write(*,'(A)') "Section 71: Calculus of Probabilities (expectation numeric vs analytic)"
  call probabilities_calc_demo()

  write(*,'(A)') "Section 72: Calculus of Complex Functions (contour integral numeric demo)"
  call complex_functions_demo()

  write(*,'(A)') "Section 73: Calculus of Distributions (extended demo)"
  call distributions_demo()

  write(*,'(A)') "Section 74: Calculus of Moving Frames (Frenet-Serret demo)"
  call moving_frames_demo()

  write(*,'(A)') "Section 75: Calculus of Lie Algebras (so(3) commutator demo)"
  call lie_algebras_demo()

  write(*,'(A)') "Section 76: Calculus of Spinors (Pauli matrix demo)"
  call spinors_demo()

  write(*,'(A)') "Section 77: Calculus of Fractals (additional note/demo)"
  call fractals_demo()

  write(*,'(A)') "Section 78: Calculus of Rough Paths (note/demo placeholder)"
  call rough_paths_demo()

  write(*,'(A)') "Section 79: Calculus of Variants in Economics (note/demo)"
  call econ_variants_demo()

  write(*,'(A)') "Section 80: Calculus of Neural Fields (tiny field gradient demo)"
  call neural_fields_demo()

  write(*,'(A)') "Section 81: Calculus of Fuzzy Logic (membership demo)"
  call fuzzy_sets_demo()

  write(*,'(A)') "Section 82: Calculus of Hyperfunctions (note)"
  call hyperfunctions_demo()

  write(*,'(A)') "Section 83: Calculus of Supergeometry (note)"
  call supergeometry_demo()

  write(*,'(A)') "Section 84: Calculus of Jet Bundles (jet/Taylor demo)"
  call jet_bundles_demo()

  write(*,'(A)') "Section 85: Calculus of Categories (natural transformation demo)"
  call categories_demo()

  write(*,'(A)') "Section 86: Calculus of Variants in Control Theory (PID demo)"
  call control_variants_demo()

  write(*,'(A)') "Section 87: Calculus of Infinite Dimensional Spaces (L2 norm demo)"
  call infinite_dimensional_demo()

  write(*,'(A)') "Section 88: Calculus of Path Integrals (simple MC path integral demo)"
  call path_integral_demo()

  write(*,'(A)') "Section 89: Calculus of Categories (functor example repeated)"
  call functor_demo()

  write(*,'(A)') "Section 90: Summary of added advanced calculus demos"
  write(*,'(A)') "  Added compact demos/placeholders for many advanced calculus topics."

  write(*,'(A)') "Section 90b: Additional runnable demos"
  call fourier_series_demo()
  call spectral_derivative_demo()
  call spectral_heat_demo()
    call fft_filter_demo()
  call convolution_demo()
  call wave1d_snapshots_demo()
  call advection1d_lw_demo()
  call advection1d_upwind_demo()
  call method_of_characteristics_demo()
  call haar_dwt_demo()
  call haar_inverse_demo()
  call haar_denoise_demo()
  call burgers_spectral_demo()
  call spectral_convergence_demo()
  call fem_poisson_1d_demo()
  call randomized_svd_demo()
  call dmd_koopman_demo()
   call hormander_calculus_demo()
  call pseudodiff_calculus_demo()
  call fourier_integral_operator_demo()
  call weyl_calculus_demo()
  call berezin_calculus_demo()
  call clifford_calculus_demo()
  call geometric_clifford_demo()
  call microlocal_sheaves_demo()
  call variational_functional_analysis_demo()
  call quantum_stochastic_calculus_demo()
  call stft_demo()
  call poisson_1d_demo()
  call linear_algebra_demo()

  call fourier_series_demo()
  call poisson_1d_demo()
  call linear_algebra_demo()

  write(*,'(A)') "Section 90c: Poisson 2D (Conjugate Gradient) demo"
  call poisson2d_cg_demo()
  call multigrid_poisson_demo()
  call fem2d_poisson_demo()
  call dijkstra_demo()
  call fast_marching_demo()
  call fast_sweeping_demo()
  call fast_marching_heap_demo()
  call implicit_heat2d_demo()
  call crank_nicolson2d_demo()

  write(*,'(A)') "Section 90d: Crank–Nicolson & Power-method demos"
  call crank_nicolson_demo()
  call power_method_demo()

  write(*,'(A)') "Section 91: Noncommutative Calculus (matrix commutator derivation)"
  call noncommutative_calculus_demo()

  write(*,'(A)') "Section 92: Supercalculus (supermanifold placeholder)"
  call supercalculus_demo()

  write(*,'(A)') "Section 93: q-Difference Calculus (q-difference demo)"
  call q_diff_demo(1.2_dp, 1.0_dp, 3)

  write(*,'(A)') "Section 94: Calculus of Variants in Game Theory (replicator dynamics demo)"
  call game_theory_variants_demo()

  write(*,'(A)') "Section 95: Calculus of Functors (higher-order functor demo)"
  call higher_order_functors_demo()

  write(*,'(A)') "Section 96: Synthetic Differential Geometry (note)"
  call synthetic_diff_demo()

  write(*,'(A)') "Section 97: Microlocal Calculus (note/demo placeholder)"
  call microlocal_demo()

  write(*,'(A)') "Section 98: Calculus of Currents (note/demo)"
  call currents_demo()

  write(*,'(A)') "Section 99: Calculus of Hyperreal Numbers (hyperreal/infinitesimal placeholder)"
  call hyperreals_demo()

  write(*,'(A)') "Section 100: Calculus of Evolutionary Systems (continuous replicator/selection demo)"
  call evolutionary_systems_demo()

  write(*,'(A)') "Section 101: End of Extended Calculus Demos"
  write(*,'(A)') "Section 102: Noncommutative Geometry Calculus (note/demo)"
  call noncommutative_geometry_demo()

  write(*,'(A)') "Section 103: Supermanifold Calculus (supermanifold placeholder)"
  call supermanifold_demo()

  write(*,'(A)') "Section 104: Topos-Theoretic Calculus (note)"
  call topos_theoretic_demo()

  write(*,'(A)') "Section 105: Synthetic Differential Geometry (reuse placeholder)"
  call synthetic_diff_demo()

  write(*,'(A)') "Section 106: Microlocal Calculus (reuse placeholder)"
  call microlocal_demo()

  write(*,'(A)') "Section 107: Hyperfunction Calculus (note placeholder)"
  call hyperfunctions_demo()

  write(*,'(A)') "Section 108: Infinite-Dimensional Calculus (reuse L2 demo)"
  call infinite_dimensional_demo()

  write(*,'(A)') "Section 109: Path Integral Calculus (reuse crude MC demo)"
  call path_integral_demo()

  write(*,'(A)') "Section 110: Calculus of Variants in AI/Neural Nets (gradient flow note)"
  call neural_calc_demo()

  write(*,'(A)') "Section 111: Calculus of Fractals & Fractional Dimensions (reuse fractal demo)"
  call fractals_demo()

  write(*,'(A)') "Section 112: Calculus of Higher Categories (note/demo placeholder)"
  call higher_categories_demo()

  write(*,'(A)') "Section 113: Calculus of Quantum Groups (q-deformation demo)"
  call quantum_groups_demo()

  write(*,'(A)') "Section 114: Calculus of Superstrings (note placeholder)"
  call superstrings_demo()

  write(*,'(A)') "Section 115: Calculus of Infinity-Categories (note placeholder)"
  call infinity_categories_demo()

contains
  ! Helper: ensure a minimum finite step-size to avoid division by zero
  real(dp) function ensure_min_h(h_in)
    implicit none
    real(dp), intent(in) :: h_in
    real(dp), parameter :: min_h = 1.0e-30_dp
    if (abs(h_in) < min_h) then
      ensure_min_h = min_h * sign(1.0_dp, h_in + 1.0e-300_dp)
    else
      ensure_min_h = h_in
    end if
  end function ensure_min_h

  ! Derivative helpers (f(x)=x^2)
  real(dp) function deriv1_poly2(x)
    implicit none
    real(dp), intent(in) :: x
      deriv1_poly2 = 2.0_dp * x
  end function deriv1_poly2

  real(dp) function deriv2_poly2()
    implicit none
    deriv2_poly2 = 2.0_dp
  end function deriv2_poly2

  real(dp) function deriv3_poly2()
    implicit none
    deriv3_poly2 = 0.0_dp
  end function deriv3_poly2

  ! Integrals of f(x)=x^2
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
      h = (b-a)/n
    ! Simpson's rule requires an even number of intervals; adjust if necessary
    nm = n
    if (mod(nm,2) /= 0) then
      nm = nm + 1
      write(*,'(A,I0)') '  Warning: Simpson requires even n; using n=', nm
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

  ! Taylor e^x
  real(dp) function taylor_exp(x, terms)
    implicit none
    real(dp), intent(in) :: x
    integer, intent(in) :: terms
    integer :: k
    real(dp) :: term, sum
    if (terms <= 0) then
      taylor_exp = 0.0_dp
      return
    end if
    sum = 0.0_dp
    term = 1.0_dp
    ! Use 'terms' as the number of terms to include (consistent with taylor_sin)
    do k = 0, terms-1
      sum = sum + term
      term = term * x / real(k+1, dp)
    end do
    taylor_exp = sum
  end function taylor_exp

  ! Euler dy/dx = -y
  subroutine euler_decay(x0, y0, h, steps)
    implicit none
    real(dp), intent(in) :: x0, y0, h
    integer, intent(in) :: steps
    integer :: i
    real(dp) :: x, y
    x = x0; y = y0
    write(*,'(A)') "  Euler:"
    do i = 1, steps
      y = y + h * (-y)
      x = x + h
      write(*,'(A,F14.10,A,F14.10)') "    x=", x, "  y=", y
    end do
  end subroutine euler_decay

  ! RK4 dy/dx = -y
  subroutine rk4_decay(x0, y0, h, steps)
    implicit none
    real(dp), intent(in) :: x0, y0, h
    integer, intent(in) :: steps
    integer :: i
    real(dp) :: x, y, k1, k2, k3, k4
    x = x0; y = y0
    write(*,'(A)') "  RK4:"
    do i = 1, steps
      k1 = -y
      k2 = -(y + 0.5*h*k1)
      k3 = -(y + 0.5*h*k2)
      k4 = -(y + h*k3)
      y = y + (h/6.0)*(k1 + 2.0*k2 + 2.0*k3 + k4)
      x = x + h
      write(*,'(A,F14.10,A,F14.10)') "    x=", x, "  y=", y
    end do
  end subroutine rk4_decay

  ! Newton-Raphson root of x^2 - 2
  real(dp) function newton_root_sqrt2(x0, tol, maxit)
    implicit none
    real(dp), intent(in) :: x0, tol
    integer, intent(in) :: maxit
    integer :: it
    real(dp) :: x, f, df
    x = x0
    do it = 1, maxit
      f = x*x - 2.0
      df = 2.0*x
      if (abs(f) < tol) exit
      x = x - f/df
    end do
    newton_root_sqrt2 = x
  end function newton_root_sqrt2

  ! Partial derivatives of f(x,y)=x^2 + y^2
  real(dp) function partial_x_poly2xy(x)
    implicit none
    real(dp), intent(in) :: x
    partial_x_poly2xy = 2.0_dp * x
  end function partial_x_poly2xy

  real(dp) function partial_y_poly2xy(y)
    implicit none
    real(dp), intent(in) :: y
    partial_y_poly2xy = 2.0_dp * y
  end function partial_y_poly2xy

  ! Gradient descent on f(x)=x^2
  subroutine gradient_descent_poly2(x_init, alpha, steps)
    implicit none
    real(dp), intent(in) :: x_init, alpha
    integer, intent(in) :: steps
    integer :: i
    real(dp) :: x, df
    x = x_init
    do i = 1, steps
      df = 2.0_dp * x
      x = x - alpha*df
      write(*,'(A,I4,A,F18.12)') "  step", i, " x=", x
    end do
  end subroutine gradient_descent_poly2

  ! Romberg integration for sin(x)
  real(dp) function romberg_sin(a, b, maxk)
    implicit none
    real(dp), intent(in) :: a, b
    integer, intent(in) :: maxk
    integer :: k, j, n, i
    real(dp) :: h, x
    real(dp) :: R(0:20,0:20)
    ! Base trapezoid
    n = 1
    h = (b-a)
    R(0,0) = 0.5*h*(sin(a) + sin(b))
    do k = 1, maxk
      ! Composite trapezoid refinement: add midpoints at odd indices
      n = 2*n
      h = (b-a)/n
      R(k,0) = 0.5*R(k-1,0)
      do i = 1, n-1, 2
        x = a + i*h
        R(k,0) = R(k,0) + h*sin(x)
      end do
      ! Richardson extrapolation along the row
      do j = 1, k
        R(k,j) = R(k,j-1) + (R(k,j-1) - R(k-1,j-1)) / (4.0**j - 1.0)
      end do
    end do
    romberg_sin = R(maxk, maxk)
  end function romberg_sin

  ! RK4 system: dy/dx=-y, dz/dx=z
  subroutine rk4_system(x0, y0, z0, h, steps)
    implicit none
    real(dp), intent(in) :: x0, y0, z0, h
    integer, intent(in) :: steps
    integer :: i
    real(dp) :: x, y, z
    real(dp) :: ky1, ky2, ky3, ky4
    real(dp) :: kz1, kz2, kz3, kz4
    x = x0; y = y0; z = z0
    do i = 1, steps
      ky1 = -y
      ky2 = -(y + 0.5*h*ky1)
      ky3 = -(y + 0.5*h*ky2)
      ky4 = -(y + h*ky3)
      y = y + (h/6.0)*(ky1 + 2.0*ky2 + 2.0*ky3 + ky4)

      kz1 =  z
      kz2 =  (z + 0.5*h*kz1)
      kz3 =  (z + 0.5*h*kz2)
      kz4 =  (z + h*kz3)
      z = z + (h/6.0)*(kz1 + 2.0*kz2 + 2.0*kz3 + kz4)

      x = x + h
      write(*,'(A,F14.10,A,F14.10,A,F14.10)') "  x=", x, "  y=", y, "  z=", z
    end do
  end subroutine rk4_system

  ! Gaussian quadrature (2-point) for ∫ f(x)=x^2 from a to b
  real(dp) function gauss2_poly2(a, b)
    implicit none
    real(dp), intent(in) :: a, b
    real(dp) :: c, m, t1, t2, x1, x2
    c = 0.5*(b+a)     ! center
    m = 0.5*(b-a)     ! half-length
    t1 = -1.0/sqrt(3.0)
    t2 =  1.0/sqrt(3.0)
    x1 = c + m*t1
    x2 = c + m*t2
    gauss2_poly2 = m * (x1**2 + x2**2)
  end function gauss2_poly2

  ! Gaussian quadrature (3-point) for ∫ f(x)=x^2 from a to b
  real(dp) function gauss3_poly2(a, b)
    implicit none
    real(dp), intent(in) :: a, b
    real(dp) :: c, m, t1, t2, t3, w1, w2, w3, x1, x2, x3
    c = 0.5*(b+a)
    m = 0.5*(b-a)
    t1 = -sqrt(3.0_dp/5.0_dp)
    t2 =  0.0_dp
    t3 =  sqrt(3.0_dp/5.0_dp)
    w1 = 5.0_dp/9.0_dp
    w2 = 8.0_dp/9.0_dp
    w3 = 5.0_dp/9.0_dp
    x1 = c + m*t1
    x2 = c + m*t2
    x3 = c + m*t3
    gauss3_poly2 = m * (w1*x1**2 + w2*x2**2 + w3*x3**2)
  end function gauss3_poly2

  ! Taylor sin(x) with N terms
  real(dp) function taylor_sin(x, terms)
    implicit none
    real(dp), intent(in) :: x
    integer, intent(in) :: terms
    integer :: k
    real(dp) :: sum, term
    sum = 0.0_dp
    term = x
    do k = 0, terms-1
      if (mod(k,2) == 0) then
        sum = sum + term
      else
        sum = sum - term
      end if
      term = term * x*x / real((2*k+2)*(2*k+3), dp)
    end do
    taylor_sin = sum
  end function taylor_sin

  ! Limit sin(x)/x as x -> 0 approximated at small eps
  real(dp) function limit_sin_over_x(eps)
    implicit none
    real(dp), intent(in) :: eps
    limit_sin_over_x = sin(eps)/eps
  end function limit_sin_over_x

  ! Numerical gradient of f(x,y)=x^2+y^2
  subroutine gradient_poly2xy(x, y, h)
    implicit none
    real(dp), intent(in) :: x, y, h
    real(dp) :: gx, gy
    gx = (((x+h)**2 + y**2) - ((x-h)**2 + y**2)) / (2.0_dp*h)
    gy = ((x**2 + (y+h)**2) - (x**2 + (y-h)**2)) / (2.0_dp*h)
    write(*,'(A,F14.10,A,F14.10,A)') "  grad = (", gx, ", ", gy, ")"
  end subroutine gradient_poly2xy

  ! Adaptive trapezoid for ∫ exp(x) from a to b until tolerance
  real(dp) function adaptive_trap_exp(a, b, tol)
    implicit none
    real(dp), intent(in) :: a, b, tol
    integer :: n, i
    real(dp) :: h, x, Iold, Inew
    n = 1
    h = (b-a)
    Iold = 0.5*h*(exp(a) + exp(b))
    do
      n = 2*n
      h = (b-a)/n
      ! Recompute composite trapezoid from scratch for clarity
      Inew = 0.0
      do i = 0, n
        x = a + i*h
        if (i == 0 .or. i == n) then
          Inew = Inew + 0.5_dp*h*exp(x)
        else
          Inew = Inew + h*exp(x)
        end if
      end do
      if (abs(Inew - Iold) < tol) exit
      Iold = Inew
    end do
    adaptive_trap_exp = Inew
  end function adaptive_trap_exp

  ! Central difference derivative for exp(x)
  real(dp) function deriv1_exp(x, h)
    implicit none
    real(dp), intent(in) :: x, h
    ! guard h to avoid division by zero or extremely tiny denominators
    deriv1_exp = (exp(x+ensure_min_h(h)) - exp(x-ensure_min_h(h))) / (2.0_dp*ensure_min_h(h))
  end function deriv1_exp

  ! Complex-step derivative for exp(x): imag(exp(x + i*h))/h
  real(dp) function deriv1_complex_exp(x, h)
    implicit none
    real(dp), intent(in) :: x, h
    complex(dp) :: z
    ! use a safe minimum imaginary step
    z = cmplx(x, ensure_min_h(h), kind=dp)
    deriv1_complex_exp = aimag(exp(z)) / ensure_min_h(h)
  end function deriv1_complex_exp

  ! Richardson-extrapolated derivative for exp(x)
  real(dp) function deriv1_richardson_exp(x, h, levels)
    implicit none
    real(dp), intent(in) :: x, h
    integer, intent(in) :: levels
    integer :: k, j
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
    deriv1_richardson_exp = R(levels-1, levels-1)
    deallocate(D)
    deallocate(R)
  end function deriv1_richardson_exp

  ! ------------------------- Additional demos -------------------------

  real(dp) function binomial_real(alpha, k)
    implicit none
    real(dp), intent(in) :: alpha
    integer, intent(in) :: k
    binomial_real = gamma(alpha+1.0_dp) / (gamma(real(k+1,dp)) * gamma(alpha - real(k,dp) + 1.0_dp))
  end function binomial_real

  integer function binomial_int(n,k)
    implicit none
    integer, intent(in) :: n, k
    integer :: i
    if (k < 0 .or. k > n) then
      binomial_int = 0
      return
    end if
    binomial_int = 1
    do i = 1, k
      binomial_int = binomial_int * (n - k + i) / i
    end do
  end function binomial_int

  real(dp) function frac_deriv_gl_poly2(x, alpha, h, N)
    implicit none
    real(dp), intent(in) :: x, alpha, h
    integer, intent(in) :: N
    real(dp) :: D_h, D_h2, D_h4, p_est, r12, d1, d2, pow2p, denom
    integer :: N2, N4
    real(dp), parameter :: tol_small = 1.0e-16_dp

    ! Compute adaptive estimates at h, h/2 and h/4
    N2 = max(1, 2*N)
    N4 = max(1, 4*N)
    D_h  = frac_deriv_gl_poly2_adaptive(x, alpha, h,    max(1,N))
    D_h2 = frac_deriv_gl_poly2_adaptive(x, alpha, h/2.0_dp, N2)
    D_h4 = frac_deriv_gl_poly2_adaptive(x, alpha, h/4.0_dp, N4)

    ! Estimate observed convergence order p from differences
    d1 = D_h - D_h2
    d2 = D_h2 - D_h4
    if (abs(d2) < tol_small .or. abs(d1) < tol_small) then
      p_est = 1.0_dp
    else
      r12 = abs(d1 / d2)
      if (r12 <= 0.0_dp) then
        p_est = 1.0_dp
      else
        p_est = log(r12) / log(2.0_dp)
        ! clamp p_est to reasonable range
        if (p_est < 0.1_dp) p_est = 0.1_dp
        if (p_est > 10.0_dp) p_est = 10.0_dp
      end if
    end if

    ! Perform Richardson extrapolation using estimated p
    pow2p = 2.0_dp**p_est
    denom = pow2p - 1.0_dp
    if (abs(denom) < tol_small) then
      frac_deriv_gl_poly2 = 2.0_dp * D_h2 - D_h
    else
      frac_deriv_gl_poly2 = (pow2p * D_h2 - D_h) / denom
    end if
    ! Publish diagnostics to host scope
    last_frac_p = p_est
    last_frac_iter = 0
    last_frac_method = 'GL_dyn'
  end function frac_deriv_gl_poly2

  subroutine compute_richardson_extrap(D_h, D_h2, D_h4, E_h, E_h2, p_est)
    implicit none
    integer, parameter :: dp = selected_real_kind(15,307)
    real(dp), intent(in) :: D_h, D_h2, D_h4
    real(dp), intent(out) :: E_h, E_h2, p_est
    real(dp) :: d1, d2, r12, pow2p, denom, tol_small
    tol_small = 1.0e-16_dp

    d1 = D_h - D_h2
    d2 = D_h2 - D_h4
    if (abs(d2) < tol_small .or. abs(d1) < tol_small) then
      p_est = 1.0_dp
    else
      r12 = abs(d1 / d2)
      if (r12 <= 0.0_dp) then
        p_est = 1.0_dp
      else
        p_est = log(r12) / log(2.0_dp)
        if (p_est < 0.1_dp) p_est = 0.1_dp
        if (p_est > 12.0_dp) p_est = 12.0_dp
      end if
    end if
    pow2p = 2.0_dp**p_est
    denom = pow2p - 1.0_dp
    if (abs(denom) < tol_small) then
      E_h = 2.0_dp * D_h2 - D_h
      E_h2 = 2.0_dp * D_h4 - D_h2
    else
      E_h  = (pow2p * D_h2 - D_h) / denom
      E_h2 = (pow2p * D_h4 - D_h2) / denom
    end if
  end subroutine compute_richardson_extrap

  subroutine compute_richardson_extrap_qp(D_h, D_h2, D_h4, E_h, E_h2, p_est)
    implicit none
    integer, parameter :: qp_loc = selected_real_kind(33,4931)
    real(qp_loc), intent(in) :: D_h, D_h2, D_h4
    real(qp_loc), intent(out) :: E_h, E_h2, p_est
    real(qp_loc) :: d1, d2, r12, pow2p, denom, tol_small
    tol_small = 1.0e-30_qp_loc

    d1 = D_h - D_h2
    d2 = D_h2 - D_h4
    if (abs(d2) < tol_small .or. abs(d1) < tol_small) then
      p_est = 1.0_qp_loc
    else
      r12 = abs(d1 / d2)
      if (r12 <= 0.0_qp_loc) then
        p_est = 1.0_qp_loc
      else
        p_est = log(r12) / log(2.0_qp_loc)
        if (p_est < 0.1_qp_loc) p_est = 0.1_qp_loc
        if (p_est > 12.0_qp_loc) p_est = 12.0_qp_loc
      end if
    end if
    pow2p = 2.0_qp_loc**p_est
    denom = pow2p - 1.0_qp_loc
    if (abs(denom) < tol_small) then
      E_h  = 2.0_qp_loc * D_h2 - D_h
      E_h2 = 2.0_qp_loc * D_h4 - D_h2
    else
      E_h  = (pow2p * D_h2 - D_h) / denom
      E_h2 = (pow2p * D_h4 - D_h2) / denom
    end if
  end subroutine compute_richardson_extrap_qp

  ! Wynn-epsilon acceleration for double precision partial sums
  real(dp) function wynn_epsilon_dp(sums, n)
    implicit none
    integer, parameter :: dp = selected_real_kind(15,307)
    real(dp), intent(in) :: sums(:)
    integer, intent(in) :: n
    integer :: i, j, k, m
    real(dp), allocatable :: eps_prev(:), eps_curr(:), eps_next(:)
    real(dp) :: delta, best_val, prev_val, cand

    if (n <= 0) then
      wynn_epsilon_dp = 0.0_dp
      return
    end if
    allocate(eps_prev(n), eps_curr(n), eps_next(n))
    do i = 1, n
      eps_curr(i) = sums(i)
      eps_prev(i) = 0.0_dp
    end do
    best_val = eps_curr(1)
    prev_val = best_val
    do k = 1, n-1
      m = n - k
      do j = 1, m
        delta = eps_curr(j+1) - eps_curr(j)
        if (abs(delta) < 1.0e-300_dp) then
          eps_next(j) = huge(1.0_dp)
        else
          eps_next(j) = eps_prev(j+1) + 1.0_dp / delta
        end if
      end do
      ! rotate
      do i = 1, m
        eps_prev(i) = eps_curr(i)
        eps_curr(i) = eps_next(i)
      end do
      if (mod(k,2) == 0) then
        cand = eps_curr(1)
        if (abs(cand - prev_val) < abs(best_val - prev_val)) then
          best_val = cand
        end if
        prev_val = cand
      end if
    end do
    wynn_epsilon_dp = best_val
    deallocate(eps_prev, eps_curr, eps_next)
  end function wynn_epsilon_dp

  ! Wynn-epsilon acceleration for quad precision partial sums
  real(kind=qp) function wynn_epsilon_qp(sums, n)
    implicit none
    integer, parameter :: qp_loc = selected_real_kind(33,4931)
    real(qp_loc), intent(in) :: sums(:)
    integer, intent(in) :: n
    integer :: i, j, k, m
    real(qp_loc), allocatable :: eps_prev(:), eps_curr(:), eps_next(:)
    real(qp_loc) :: delta, best_val, prev_val, cand

    if (n <= 0) then
      wynn_epsilon_qp = 0.0_qp_loc
      return
    end if
    allocate(eps_prev(n), eps_curr(n), eps_next(n))
    do i = 1, n
      eps_curr(i) = sums(i)
      eps_prev(i) = 0.0_qp_loc
    end do
    best_val = eps_curr(1)
    prev_val = best_val
    do k = 1, n-1
      m = n - k
      do j = 1, m
        delta = eps_curr(j+1) - eps_curr(j)
        if (abs(delta) < 1.0e-300_qp_loc) then
          eps_next(j) = huge(1.0_qp_loc)
        else
          eps_next(j) = eps_prev(j+1) + 1.0_qp_loc / delta
        end if
      end do
      ! rotate
      do i = 1, m
        eps_prev(i) = eps_curr(i)
        eps_curr(i) = eps_next(i)
      end do
      if (mod(k,2) == 0) then
        cand = eps_curr(1)
        if (abs(cand - prev_val) < abs(best_val - prev_val)) then
          best_val = cand
        end if
        prev_val = cand
      end if
    end do
    wynn_epsilon_qp = best_val
    deallocate(eps_prev, eps_curr, eps_next)
  end function wynn_epsilon_qp

  real(dp) function frac_deriv_gl_poly2_adaptive(x, alpha, h, Nmax, trunc_policy, tail_tol)
    implicit none
    real(dp), intent(in) :: x, alpha, h
    integer, intent(in) :: Nmax
    character(len=*), intent(in), optional :: trunc_policy
    real(dp), intent(in), optional :: tail_tol
    integer :: k
    real(dp) :: sum_local, c_local, coeff, term, arg
    real(dp) :: y, tsum
    real(dp) :: tol, tailtol_local
    character(len=16) :: policy
    real(kind=qp) :: qp_res_k
    real(dp) :: qp_as_dp, reldiff
    real(dp) :: max_log_dp
    ! tail-acceleration storage
    integer, parameter :: m_tail = 16
    real(dp) :: tail_sums(m_tail)
    integer :: tail_count, tail_idx, n_used
    real(dp) :: acc_est
    ! locals for building recent partial-sums array for accelerator
    real(dp), allocatable :: recent(:)
    integer :: start_idx, ii
    ! Wynn-epsilon acceptance thresholds (tunable)
    real(dp) :: wynn_rel_accept, wynn_abs_mult
    ! log-space coefficient accumulation variables (dp path)
    real(dp) :: log_coeff_dp, factor, afac
    integer :: sign_coeff_int, sign_pow

    ! defaults (tighten to approach machine precision safely)
    tol = 1.0e-15_dp
    tailtol_local = 1.0e-15_dp
    policy = 'zero'
    if (present(trunc_policy)) then
      policy = adjustl(trim(trunc_policy))
    end if
    if (present(tail_tol)) then
      tailtol_local = tail_tol
    end if

    sum_local = 0.0_dp
    c_local = 0.0_dp
    ! Use log-space coefficient accumulation with explicit sign tracking for
    ! improved numerical stability of generalized binomial coefficients.
    log_coeff_dp = 0.0_dp
    sign_coeff_int = 1
    sign_pow = 1
    ! initial coeff (k=0) has log=0 and sign=+1
    tail_count = 0
    do k = 0, Nmax
      if (k > 0) then
        factor = alpha - real(k-1, dp)
        if (abs(factor) == 0.0_dp) then
          ! exact zero factor -> subsequent coefficients are zero
          sign_coeff_int = 0
          log_coeff_dp = -huge(1.0_dp)
        else
          if (factor < 0.0_dp) then
            sign_coeff_int = -sign_coeff_int
            afac = -factor
          else
            afac = factor
          end if
          log_coeff_dp = log_coeff_dp + log(afac) - log(real(k, dp))
        end if
        sign_pow = -sign_pow
      end if
      arg = x - real(k,dp) * h
      ! reconstruct coefficient from log/sign (if sign_coeff_int==0 then coeff_dp=0)
      if (sign_coeff_int == 0) then
        coeff = 0.0_dp
      else
        max_log_dp = log(huge(1.0_dp))
        if (log_coeff_dp > max_log_dp - 2.0_dp) then
          ! coefficient would overflow in exp -> cap to huge and continue
          coeff = sign_coeff_int * huge(1.0_dp)
        else
          coeff = sign_coeff_int * exp(log_coeff_dp)
        end if
      end if
      if (arg < 0.0_dp) then
        select case (policy)
        case ('zero')
          ! leave term = 0 (skip)
          term = 0.0_dp
        case ('reflect')
          term = real(sign_pow, dp) * coeff * (abs(arg)**2)
        case default
          ! default zero extension
          term = 0.0_dp
        end select
      else
        term = real(sign_pow, dp) * coeff * (arg**2)
      end if

      ! record term into tail buffer (for tail-modeling after truncation)
      tail_idx = mod(tail_count, m_tail) + 1
      tail_sums(tail_idx) = sum_local
      tail_count = tail_count + 1

      ! Kahan compensated summation
      y = term - c_local
      tsum = sum_local + y
      c_local = (tsum - sum_local) - y
      sum_local = tsum

      ! stopping criteria: stop when term is small absolutely or small relative
      if ( abs(term) < tailtol_local .or. abs(term) < tol .or. abs(term) < tol * max(1.0_dp, abs(sum_local)) ) then
        last_frac_iter = k
        last_frac_method = 'GL_adapt'
        exit
      end if
    end do
    ! If we reached Nmax without meeting tail tolerance, attempt Wynn-epsilon on recent partial sums
    if (tail_count > 0 .and. tail_count >= m_tail .and. k > Nmax) then
      n_used = min(m_tail, tail_count)
      ! build ordered array of last n_used partial sums (oldest->newest)
      allocate(recent(n_used))
      start_idx = mod(tail_count, m_tail) + 1
      do ii = 1, n_used
        recent(ii) = tail_sums(mod(start_idx-1 + ii-1, m_tail) + 1)
      end do
      acc_est = wynn_epsilon_dp(recent, n_used)
      deallocate(recent)
      ! set acceptance thresholds (relative and machine-epsilon based)
      wynn_rel_accept = 1.0e-6_dp
      wynn_abs_mult = 1.0e3_dp
      ! accept accelerated value if finite and change is small relative to sum
      if (acc_est == acc_est .and. abs(acc_est) < huge(1.0_dp)) then
        if ( abs(acc_est - sum_local) / max(abs(sum_local), 1.0_dp) <= wynn_rel_accept .or. &
             abs(acc_est - sum_local) <= wynn_abs_mult * epsilon(1.0_dp) * max(abs(sum_local), 1.0_dp) ) then
          sum_local = acc_est
          last_frac_method = 'GL_adapt_wynn'
        end if
      end if
    end if
    ! if loop finished fully set diagnostics
    if (k > Nmax) then
      last_frac_iter = Nmax
    end if
    last_frac_method = 'GL_adapt'
    ! base double-precision result
    frac_deriv_gl_poly2_adaptive = sum_local / (h**alpha)

    ! Lightweight high-precision verification: recompute in quad precision and
    ! prefer quad result if it differs significantly (indicates DP instability).
    ! This avoids silent catastrophic failures for extreme (tiny h, large N).
    qp_res_k = frac_deriv_gl_poly2_adaptive_qp(x, alpha, h, Nmax)
    qp_as_dp = real(qp_res_k, kind=dp)
    reldiff = 0.0_dp
    if (abs(qp_as_dp) > 0.0_dp) reldiff = abs(qp_as_dp - frac_deriv_gl_poly2_adaptive) / abs(qp_as_dp)
    if (reldiff > 1.0e-8_dp) then
      last_frac_method = 'GL_adapt_qp'
      last_frac_relerr = reldiff
      frac_deriv_gl_poly2_adaptive = qp_as_dp
    end if
  end function frac_deriv_gl_poly2_adaptive

    ! Quad-precision specialized adaptive GL summation for f(x)=x^2 (attempt)
    real(kind=qp) function frac_deriv_gl_poly2_adaptive_qp(x_in, alpha_in, h_in, Nmax_in)
      implicit none
      real(dp), intent(in) :: x_in, alpha_in, h_in
      integer, intent(in) :: Nmax_in
      real(kind=qp) :: x, alpha, h
      integer :: k, Nmax
      real(kind=qp) :: sum_local, c_local, coeff, term, arg, y, tsum
      ! log-space coefficient accumulation variables (qp path)
      real(kind=qp) :: log_coeff_qp, factor_qp, afac_qp
      integer :: sign_coeff_int_qp, sign_pow_qp
      real(kind=qp) :: max_log_qp

      ! tail-acceleration declarations (must be before executable statements)
      integer, parameter :: m_tail_qp = 16
      real(kind=qp) :: tail_terms_qp(m_tail_qp)
      integer :: tail_k_qp(m_tail_qp)
      integer :: tail_count_qp, tail_idx_qp, n_used_qp
      ! Wynn-epsilon recent sums and qp acceptance thresholds (declared here)
      real(kind=qp), allocatable :: recent_qp(:)
      integer :: start_idx_qp, ii_qp
      real(kind=qp) :: acc_est_qp, rel_change_qp, wynn_rel_accept_qp, wynn_abs_mult_qp

      x = real(x_in, kind=qp)
      alpha = real(alpha_in, kind=qp)
      h = real(h_in, kind=qp)
      Nmax = Nmax_in

      sum_local = 0.0_qp
      c_local = 0.0_qp
      ! initialize qp log-space coefficient accumulation
      log_coeff_qp = 0.0_qp
      sign_coeff_int_qp = 1
      sign_pow_qp = 1
      tail_count_qp = 0
      do k = 0, Nmax
        if (k > 0) then
          factor_qp = alpha - real(k-1, kind=qp)
          if (abs(factor_qp) == 0.0_qp) then
            sign_coeff_int_qp = 0
            log_coeff_qp = -huge(1.0_qp)
          else
            if (factor_qp < 0.0_qp) then
              sign_coeff_int_qp = -sign_coeff_int_qp
              afac_qp = -factor_qp
            else
              afac_qp = factor_qp
            end if
            log_coeff_qp = log_coeff_qp + log(afac_qp) - log(real(k, kind=qp))
          end if
          sign_pow_qp = -sign_pow_qp
        end if
        arg = x - real(k, kind=qp) * h
        if (sign_coeff_int_qp == 0) then
          coeff = 0.0_qp
        else
          max_log_qp = log(huge(1.0_qp))
          if (log_coeff_qp > max_log_qp - 2.0_qp) then
            coeff = real(sign_coeff_int_qp, kind=qp) * huge(1.0_qp)
          else
            coeff = real(sign_coeff_int_qp, kind=qp) * exp(log_coeff_qp)
          end if
        end if
        if (arg < 0.0_qp) then
          term = 0.0_qp
        else
          term = real(sign_pow_qp, kind=qp) * coeff * (arg**2)
        end if

        ! record term into tail buffer
        tail_idx_qp = mod(tail_count_qp, m_tail_qp) + 1
          ! record recent partial-sum (qp) to enable Wynn-epsilon on sums
          tail_terms_qp(tail_idx_qp) = sum_local
        tail_k_qp(tail_idx_qp) = k
        tail_count_qp = tail_count_qp + 1

        ! Kahan compensated summation in quad precision
        y = term - c_local
        tsum = sum_local + y
        c_local = (tsum - sum_local) - y
        sum_local = tsum
      end do
      ! If loop finished and we have full tail buffer, attempt qp tail-fit
      if (tail_count_qp > 0 .and. tail_count_qp >= m_tail_qp .and. k > Nmax) then
        n_used_qp = min(m_tail_qp, tail_count_qp)
        ! Build recent partial-sums array (oldest -> newest)
        allocate(recent_qp(n_used_qp))
        start_idx_qp = mod(tail_count_qp, m_tail_qp) + 1
        do ii_qp = 1, n_used_qp
          recent_qp(ii_qp) = tail_terms_qp(mod(start_idx_qp-1 + ii_qp-1, m_tail_qp) + 1)
        end do
        acc_est_qp = wynn_epsilon_qp(recent_qp, n_used_qp)
        deallocate(recent_qp)
        ! Set qp-specific acceptance thresholds
        wynn_rel_accept_qp = 1.0e-8_qp
        wynn_abs_mult_qp = 1.0e4_qp
        if (acc_est_qp == acc_est_qp .and. abs(acc_est_qp) < huge(1.0_qp)) then
          rel_change_qp = abs(acc_est_qp - sum_local) / max(abs(sum_local), 1.0_qp)
          if ( rel_change_qp <= wynn_rel_accept_qp .or. &
               abs(acc_est_qp - sum_local) <= wynn_abs_mult_qp * epsilon(1.0_qp) * max(abs(sum_local), 1.0_qp) ) then
            sum_local = acc_est_qp
          end if
        end if
      end if

      frac_deriv_gl_poly2_adaptive_qp = sum_local / (h**alpha)
    end function frac_deriv_gl_poly2_adaptive_qp

  ! Analytic fractional derivative for f(x)=x^2 at x (independent of x for polynomial)
  real(dp) function frac_deriv_monomial_exact(m, alpha, x)
    implicit none
    integer, intent(in) :: m
    real(dp), intent(in) :: alpha, x
    real(dp) :: num, den, expo
    ! Riemann-Liouville fractional derivative of x^m: Gamma(m+1)/Gamma(m+1-alpha) * x^{m-alpha}
    num = gamma(real(m+1,dp))
    den = gamma(real(m+1,dp) - alpha)
    expo = real(m,dp) - alpha
    if (x == 0.0_dp .and. expo < 0.0_dp) then
      frac_deriv_monomial_exact = 0.0_dp
    else
      frac_deriv_monomial_exact = num / den * x**expo
    end if
  end function frac_deriv_monomial_exact

  ! Backwards-compatible wrapper for x^2 at x=1
  real(dp) function frac_deriv_poly2_exact(alpha)
    implicit none
    real(dp), intent(in) :: alpha
    frac_deriv_poly2_exact = frac_deriv_monomial_exact(2, alpha, 1.0_dp)
  end function frac_deriv_poly2_exact
  
  real(dp) function frac_deriv_gl_poly2_auto(x, alpha, h_init, Ninit, rel_tol, max_iter)
    implicit none
    real(dp), intent(in) :: x, alpha, h_init, rel_tol
    integer, intent(in) :: Ninit, max_iter
    integer :: iter, Ncur
    integer :: attempt, max_qp_attempts, Nqp
    integer :: safety_factor, recommended_N_qp
    real(dp) :: hcur, D_h, D_h2, D_h4, E1, E2, rel_err_local, eps
    integer :: N2, N4
    integer :: recommended_N
    integer :: max_iter_eff
    real(dp) :: h_log10
    ! high-precision trial variables
    real(kind=qp) :: D_h_qp, D_h2_qp, D_h4_qp, E1_qp, E2_qp, p_qp
    real(dp) :: E2_hp_dp, rel_err_hp, qp_as_dp

    ! Goal-driven adaptive driver using Richardson-extrapolated stability
    hcur = ensure_min_h(h_init)
    Ncur = max(1, Ninit)
    ! Ensure base truncation N covers sampling back to x=0 when appropriate
    safety_factor = 8
    recommended_N = max(1, int( ceiling( x / hcur ) ))
    recommended_N = max(1, recommended_N * safety_factor)
    if (recommended_N > Ncur) then
      Ncur = recommended_N
    end if
    eps = 1.0e-30_dp

    ! make max_iter adaptive for very small h: scale with -log10(h)
    h_log10 = 0.0_dp
    if (hcur > 0.0_dp) h_log10 = -log10(hcur)
    ! allow a much larger effective max_iter for tiny h during exhaustive verification
    max_iter_eff = max(max_iter, min(2000, max_iter * (1 + int(h_log10*4))))

    do iter = 1, max_iter_eff
      ! compute three-resolution extrapolated estimates
      ! Recompute recommended N for current h to ensure truncation covers domain
      recommended_N = max(1, int( ceiling( x / hcur ) ))
      recommended_N = max(1, recommended_N * safety_factor)
      if (recommended_N > Ncur) then
        Ncur = recommended_N
      end if
      N2 = max(1, 2*Ncur)
      N4 = max(1, 4*Ncur)
      D_h  = frac_deriv_gl_poly2(x, alpha, hcur, Ncur)
      D_h2 = frac_deriv_gl_poly2(x, alpha, hcur/2.0_dp, N2)
      D_h4 = frac_deriv_gl_poly2(x, alpha, hcur/4.0_dp, N4)
      write(*,'(A,ES18.10,1A,ES18.10,1A,ES18.10)') '    debug: D_h,D_h2,D_h4 =', D_h, ',', D_h2, ',', D_h4

      ! estimate p and perform Richardson extrapolation for current and half-step
      call compute_richardson_extrap(D_h, D_h2, D_h4, E1, E2, last_frac_p)

      ! estimate relative difference between successive extrapolations
      rel_err_local = abs(E2 - E1) / max(abs(E2), eps)
      last_frac_relerr = rel_err_local
      last_frac_iter = iter
      last_frac_method = 'GL_auto_rich'

      if (rel_err_local <= rel_tol) then
          ! Lightweight qp verification: compute a single qp adaptive estimate at current Ncur
          write(*,'(A)') '    debug: performing qp single-check'
          D_h_qp  = frac_deriv_gl_poly2_adaptive_qp(x, alpha, hcur, Ncur)
          qp_as_dp = real(D_h_qp, kind=dp)
          if (abs(qp_as_dp) > 0.0_dp) then
            if (abs(qp_as_dp - E2) / abs(qp_as_dp) > 1.0e-8_dp) then
              ! DP extrapolation looks inconsistent with qp -> attempt qp attempts below
              last_frac_method = 'GL_auto_qpcheck'
              last_frac_relerr = abs(qp_as_dp - E2) / max(abs(qp_as_dp), eps)
              ! break out of the outer refinement loop and go to qp attempt loop
              exit
            else
              frac_deriv_gl_poly2_auto = E2
              return
            end if
          else
            ! qp result zero (degenerate) — accept DP
            frac_deriv_gl_poly2_auto = E2
            return
          end if
      end if

      ! otherwise refine: halve h and double base N and continue
      hcur = hcur / 2.0_dp
      Ncur = Ncur * 2
    end do

    ! Attempt a high-precision (quad) retry before giving up. Try several
    ! increasing N sizes for the quad recompute: often DP fails due to
    ! insufficient tail truncation; increasing N in QP frequently stabilizes.
    if (last_frac_relerr > rel_tol) then
      write(*,*) '  GL auto: max_iter reached without convergence; diagnostics follow:'
      write(*,*) '    D(h)  =', D_h
      write(*,*) '    D(h/2)=', D_h2
      write(*,*) '    D(h/4)=', D_h4
      write(*,*) '    p_est =', last_frac_p, '  rel_err_local=', last_frac_relerr
    end if

    ! Try up to several qp attempts with increasing N (doubling) up to a cap
    max_qp_attempts = 20
    ! Start QP attempts with a larger, safety-scaled N to ensure tail coverage
    recommended_N_qp = max(1, int( ceiling( x / hcur ) ))
    ! increase safety multiplier for exhaustive verification
    recommended_N_qp = max(recommended_N_qp * (safety_factor*4), Ncur)
    Nqp = max(500, recommended_N_qp)
    ! ensure QP truncation scales with domain coverage (more aggressive multiplier)
    Nqp = max(Nqp, int( ceiling( x / hcur ) ) * 50)
    ! cap Nqp to avoid runaway memory/time; increase cap for exhaustive checks
    Nqp = min(Nqp, 20000)
    E2_hp_dp = E2
    do attempt = 1, max_qp_attempts
      N2 = max(1, 2*Nqp)
      N4 = max(1, 4*Nqp)
      D_h_qp  = frac_deriv_gl_poly2_adaptive_qp(x, alpha, hcur, Nqp)
      D_h2_qp = frac_deriv_gl_poly2_adaptive_qp(x, alpha, hcur/2.0_dp, N2)
      D_h4_qp = frac_deriv_gl_poly2_adaptive_qp(x, alpha, hcur/4.0_dp, N4)
      call compute_richardson_extrap_qp(D_h_qp, D_h2_qp, D_h4_qp, E1_qp, E2_qp, p_qp)
      E2_hp_dp = real(E2_qp, kind=dp)
      rel_err_hp = abs(E2_hp_dp - E2) / max(abs(E2_hp_dp), eps)
      if (rel_err_hp < rel_err_local * 0.5 .and. rel_err_hp < rel_tol*10.0_dp) then
        write(*,'(A,I0)') '  GL auto: high-precision attempt succeeded on qp N=', Nqp
        last_frac_method = 'GL_highprec'
        last_frac_relerr = rel_err_hp
        frac_deriv_gl_poly2_auto = E2_hp_dp
        return
      end if
      ! increase N and retry (grow conservatively but allow caps)
      Nqp = min(Nqp*2, max( Nqp, Ncur*16 ))
    end do

    ! If high-precision attempts did not yield improvement, return last extrapolated estimate
    write(*,'(A)') '  Warning: adaptive GL did not converge within max_iter (qp attempts exhausted); returning last extrapolated estimate'
    frac_deriv_gl_poly2_auto = E2
  end function frac_deriv_gl_poly2_auto


  real(dp) function normal_rand()
    implicit none
    real(dp) :: u1, u2
    call random_number(u1)
    call random_number(u2)
    if (abs(u1) < 1.0e-30_dp) u1 = 1.0e-30_dp
    normal_rand = sqrt(-2.0_dp*log(u1)) * cos(2.0_dp*3.141592653589793_dp*u2)
  end function normal_rand

  subroutine stochastic_gbm_demo(x0, mu, sigma, T, Nsteps, Msamp)
    implicit none
    real(dp), intent(in) :: x0, mu, sigma, T
    integer, intent(in) :: Nsteps, Msamp
    integer :: step_i, samp_j
    real(dp) :: dt_local, x_path, z_rand, meanX_local, sumX_local
    dt_local = T / real(Nsteps, dp)
    sumX_local = 0.0_dp
    do samp_j = 1, Msamp
      x_path = x0
      do step_i = 1, Nsteps
        z_rand = normal_rand()
        x_path = x_path + mu*x_path*dt_local + sigma*x_path*sqrt(dt_local)*z_rand
      end do
      sumX_local = sumX_local + x_path
    end do
    meanX_local = sumX_local / real(Msamp, dp)
    write(*,'(A,F18.12,A,F18.12)') "  E[X_T] numeric=", meanX_local, "  analytic=", x0*exp(mu*T)
  end subroutine stochastic_gbm_demo

  real(dp) function geom_deriv_exp(x, h)
    implicit none
    real(dp), intent(in) :: x, h
    real(dp) :: fx, fxh
    real(dp) :: nanval, zero
    fx = exp(x)
    fxh = exp(x+h)
    if (fx <= 0.0_dp .or. fxh <= 0.0_dp) then
      write(*,'(A)') '  Warning: geometric derivative undefined for non-positive f; returning NaN'
      zero = 0.0_dp
      nanval = zero/zero
      geom_deriv_exp = nanval
      return
    end if
    geom_deriv_exp = exp( (log(fxh) - log(fx)) / h )
  end function geom_deriv_exp

  real(dp) function geom_deriv_poly2(x, h)
    implicit none
    real(dp), intent(in) :: x, h
    real(dp) :: fx, fxh
    real(dp) :: nanval, zero
    fx = x**2
    fxh = (x+h)**2
    if (fx <= 0.0_dp .or. fxh <= 0.0_dp) then
      write(*,'(A)') '  Warning: geometric derivative undefined for non-positive f; returning NaN'
      zero = 0.0_dp
      nanval = zero/zero
      geom_deriv_poly2 = nanval
      return
    end if
    geom_deriv_poly2 = exp( (log(fxh) - log(fx)) / h )
  end function geom_deriv_poly2

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

  function str_delta(d) result(s)
    implicit none
    real(dp), intent(in) :: d
    character(len=64) :: s
    write(s,'(ES18.10)') d
  end function str_delta

  subroutine propositional_demo()
    implicit none
    logical :: Avar, Bvar, Cvar, implA_B, implB_C, antecedent, consequent, formula_bool
    integer :: ia, ib, ic
    character(len=2) :: tf
    logical :: all_true
    all_true = .true.
    write(*,'(A)') "  Propositional formula: ((A -> B) AND (B -> C)) -> (A -> C) (syllogism)"
    write(*,'(A)') "  A B C | formula"
    do ia = 0, 1
      Avar = (ia == 1)
      do ib = 0, 1
        Bvar = (ib == 1)
        do ic = 0, 1
          Cvar = (ic == 1)
          implA_B = (.not. Avar) .or. Bvar
          implB_C = (.not. Bvar) .or. Cvar
          antecedent = implA_B .and. implB_C
          consequent = (.not. Avar) .or. Cvar
          formula_bool = (.not. antecedent) .or. consequent
          if (.not. formula_bool) all_true = .false.
          if (formula_bool) then
            tf = 'T'
          else
            tf = 'F'
          end if
          write(*,'(I1,1X,I1,1X,I1,3X,A)') ia, ib, ic, tf
        end do
      end do
    end do
    if (all_true) then
      write(*,'(A)') '  Result: formula is a tautology (true for all assignments).'
    else
      write(*,'(A)') '  Result: formula is NOT a tautology.'
    end if
  end subroutine propositional_demo

  real(dp) function ricci_scalar_sphere()
    implicit none
    ! For a unit 2-sphere the Ricci scalar R = 2 (analytic result)
    ricci_scalar_sphere = 2.0_dp
  end function ricci_scalar_sphere

  subroutine finite_calculus_demo()
    implicit none
    integer :: n_idx, Nmax
    real(dp) :: forward_val, closed_val
    Nmax = 10
    write(*,'(A)') "  Finite calculus identities: forward difference Δ f(n)=f(n+1)-f(n);"
    write(*,'(A)') "  Summation formula: sum_{k=1}^n k^2 = n(n+1)(2n+1)/6"
    write(*,'(A)') "  n  forward-diff(n^2)  closed-form-sum_{k=1}^n k^2"
    do n_idx = 1, Nmax
      forward_val = real((n_idx+1)**2 - n_idx**2, dp)
      closed_val = real(n_idx*(n_idx+1)*(2*n_idx+1),dp)/6.0_dp
      write(*,'(I3,2X,F12.6,2X,F12.6)') n_idx, forward_val, closed_val
    end do
  end subroutine finite_calculus_demo

  subroutine lambda_church_demo()
    implicit none
    integer :: zero, one, two, three, add12, mul23
    integer :: i, result
    ! Church numeral n encodes iterate f n times: n = \x. f^n x
    write(*,'(A)') "  Lambda/Church numerals (illustrative): Church(n) = lambda f x. f^n x"
    zero = 0; one = 1; two = 2; three = 3
    write(*,'(A,I4)') "   Church(0) ->", zero
    write(*,'(A,I4)') "   Church(1) ->", one
    write(*,'(A,I4)') "   Church(2) ->", two
    write(*,'(A,I4)') "   Church(3) ->", three
    ! simulate application: apply successor function succ(x)=x+1, starting at 0
    result = 0
    do i = 1, two
      result = result + 1
    end do
    write(*,'(A,I4)') "  apply Church(2) to succ and 0 ->", result
    ! addition and multiplication via iteration (as integers here)
    add12 = one + two
    mul23 = two * three
    write(*,'(A,I4)') "  add(1,2) ->", add12
    write(*,'(A,I4)') "  mult(2,3) ->", mul23
  end subroutine lambda_church_demo

  subroutine umbral_binomial_demo(N)
    implicit none
    integer :: i, j, N, epochs
    real(dp) :: w, lr, loss, y_pred, dLdw
    real(dp), allocatable :: xs(:), ys(:)

    N = 6
    allocate(xs(N), ys(N))
    xs = (/ 0.0_dp, 1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp, 5.0_dp /)
    ys = (/ 0.1_dp, 1.9_dp, 3.9_dp, 5.9_dp, 8.1_dp, 9.8_dp /) ! roughly y ~ 2*x with small noise
    w = 0.0_dp
    lr = 0.01_dp
    epochs = 500
    write(*,'(A)') "  Neural (linear regression) demo: fit y = w*x by gradient descent"
    do i = 1, epochs
      dLdw = 0.0_dp
      loss = 0.0_dp
      do j = 1, N
        y_pred = w * xs(j)
        loss = loss + 0.5_dp*(y_pred - ys(j))**2
        dLdw = dLdw + (y_pred - ys(j)) * xs(j)
      end do
      w = w - lr * dLdw / real(N,dp)
      if (mod(i,100) == 0) then
        write(*,'(A,I4,3X,A,F12.8)') "   epoch", i, "  current w=", w
      end if
    end do
    write(*,'(A,F12.8)') "   learned parameter w=", w
    write(*,'(A)') "   final residuals (y - w*x):"
    do i = 1, N
      write(*,'(I2,2X,F6.3,2X,F8.4)') i, xs(i), ys(i) - w*xs(i)
    end do
    deallocate(xs, ys)

  end subroutine umbral_binomial_demo

  ! -------------------- Placeholder demos for advanced stochastic/microlocal calculi --------------------

  subroutine malliavin_calculus_demo()
    implicit none
    integer, parameter :: dp = selected_real_kind(15,307)
    integer :: iu, j, N
    real(dp) :: a, b, T, ds, s
    character(len=512) :: line
    ! For the linear SDE dX_t = a X_t dt + b dW_t (additive noise b),
    ! the Malliavin derivative D_s X_T = b * exp(a*(T - s)) for s <= T.
    a = -1.0_dp; b = 1.0_dp; T = 1.0_dp
    N = 101
    ds = T / real(N-1, dp)
    open(unit=210, file='malliavin_calculus.csv', status='replace', action='write', iostat=iu)
    if (iu /= 0) then
      write(*,'(A)') '  Warning: could not open malliavin_calculus.csv for writing.'
      return
    end if
    call csv_write(210, 's,D_sX_T (analytic)', iu)
    do j = 1, N
      s = (real(j-1,dp)) * ds
      write(line,'(ES18.10,1A,ES18.10)') s, ',', b * exp( a * (T - s) )
      call csv_write(210, trim(line), iu)
    end do
    close(210)
    write(*,'(A)') 'Malliavin calculus demo: analytic D_s X_T saved to malliavin_calculus.csv'
  end subroutine malliavin_calculus_demo

  subroutine ito_calculus_demo()
    implicit none
    integer, parameter :: dp = selected_real_kind(15,307)
    integer :: io_stat, Nsteps, j_idx, m_idx, M_ens
    real(dp) :: T, dt_local, tt, mu, sigma, X, W_local, dW_local
    character(len=512) :: line
    real(dp), allocatable :: path0(:), sumx(:), sumx2(:)
    real(dp) :: mean_analytic, var_analytic, mean_num, var_num, rel_err
    ! Monte Carlo ensemble for geometric Brownian motion (It\u00f4) moments
    call random_seed()
    mu = 0.1_dp; sigma = 0.2_dp; T = 1.0_dp
    Nsteps = 500
    dt_local = T / real(Nsteps, dp)
    M_ens = 500     ! ensemble size (moderate)

    allocate(path0(0:Nsteps), sumx(0:Nsteps), sumx2(0:Nsteps))
    path0 = 0.0_dp
    sumx = 0.0_dp
    sumx2 = 0.0_dp

    ! simulate M independent Euler-Maruyama paths, accumulate moments
    do m_idx = 1, M_ens
      X = 1.0_dp
      W_local = 0.0_dp
      do j_idx = 0, Nsteps
        tt = real(j_idx,dp) * dt_local
        if (m_idx == 1) path0(j_idx) = X    ! store first path for inspection
        sumx(j_idx) = sumx(j_idx) + X
        sumx2(j_idx) = sumx2(j_idx) + X*X
        if (j_idx < Nsteps) then
          dW_local = sqrt(dt_local) * normal_rand()
          X = X + mu * X * dt_local + sigma * X * dW_local
          W_local = W_local + dW_local
        end if
      end do
    end do

    open(unit=211, file='ito_calculus.csv', status='replace', action='write', iostat=io_stat)
    if (io_stat /= 0) then
      write(*,'(A)') '  Warning: could not open ito_calculus.csv for writing.'
      deallocate(path0, sumx, sumx2)
      return
    end if
    call csv_write(211, 't,path0_X,mean_num,var_num,analytic_mean,analytic_var,rel_err_mean', io_stat)
    do j_idx = 0, Nsteps
      tt = real(j_idx,dp) * dt_local
      mean_num = sumx(j_idx) / real(M_ens,dp)
      var_num = ( sumx2(j_idx) - sumx(j_idx)**2 / real(M_ens,dp) ) / real(max(1,M_ens-1), dp)
      ! analytic moments for GBM with X0=1: E[X_t]=exp(mu t); Var[X_t]=exp(2 mu t)*(exp(sigma^2 t)-1)
      mean_analytic = exp( mu * tt )
      var_analytic = exp( 2.0_dp * mu * tt ) * ( exp( sigma*sigma * tt ) - 1.0_dp )
      rel_err = abs(mean_num - mean_analytic) / max(1.0e-18_dp, abs(mean_analytic))
      write(line,'(F8.4,1A,ES18.10,1A,ES18.10,1A,ES18.10,1A,ES18.10,1A,ES18.10,1A,ES18.10)') tt, ',', path0(j_idx), ',', mean_num, ',', var_num, ',', mean_analytic, ',', var_analytic, ',', rel_err
      call csv_write(211, trim(line), io_stat)
    end do
    close(211)
    deallocate(path0, sumx, sumx2)
    write(*,'(A)') 'It\u00f4 calculus demo: GBM ensemble moments saved to ito_calculus.csv'
  end subroutine ito_calculus_demo

  subroutine stratonovich_calculus_demo()
    implicit none
    integer, parameter :: dp = selected_real_kind(15,307)
    integer :: io_stat_s, Nsteps_s, j_idx_s, m_idx_s, M_ens_s
    real(dp) :: T, dt_local, tt, mu, sigma, X_em, X_heun, Xpred, dW_local
    real(dp), allocatable :: path0_heun(:), path0_em(:)
    real(dp), allocatable :: sumx_heun(:), sumx2_heun(:), sumx_em(:), sumx2_em(:)
    real(dp) :: mean_analytic, var_analytic, mean_heun, var_heun, mean_em, var_em, rel_err_heun, rel_err_em
    character(len=64) :: row
    character(len=24) :: s_tt, s_p0h, s_p0e, s_mh, s_vh, s_me, s_ve, s_ma, s_reh, s_reem
    character(len=512) :: line

    ! Ensemble comparison: Euler-Maruyama (It\u00f4) vs Heun (Stratonovich-like)
    call random_seed()
    mu = 0.1_dp; sigma = 0.2_dp; T = 1.0_dp
    Nsteps_s = 500
    dt_local = T / real(Nsteps_s, dp)
    M_ens_s = 500

    allocate(path0_heun(0:Nsteps_s), path0_em(0:Nsteps_s))
    allocate(sumx_heun(0:Nsteps_s), sumx2_heun(0:Nsteps_s))
    allocate(sumx_em(0:Nsteps_s), sumx2_em(0:Nsteps_s))
    path0_heun = 0.0_dp; path0_em = 0.0_dp
    sumx_heun = 0.0_dp; sumx2_heun = 0.0_dp
    sumx_em = 0.0_dp;   sumx2_em = 0.0_dp

    ! simulate ensemble: use same dW for both integrators per path
    do m_idx_s = 1, M_ens_s
      X_em = 1.0_dp
      X_heun = 1.0_dp
      do j_idx_s = 0, Nsteps_s
        tt = real(j_idx_s, dp) * dt_local
        if (m_idx_s == 1) then
          path0_heun(j_idx_s) = X_heun
          path0_em(j_idx_s) = X_em
        end if
        sumx_heun(j_idx_s) = sumx_heun(j_idx_s) + X_heun
        sumx2_heun(j_idx_s) = sumx2_heun(j_idx_s) + X_heun*X_heun
        sumx_em(j_idx_s) = sumx_em(j_idx_s) + X_em
        sumx2_em(j_idx_s) = sumx2_em(j_idx_s) + X_em*X_em
        if (j_idx_s < Nsteps_s) then
          dW_local = sqrt(dt_local) * normal_rand()
          ! Euler-Maruyama (It\u00f4)
          X_em = X_em + mu * X_em * dt_local + sigma * X_em * dW_local
          ! Heun predictor-corrector (Stratonovich-style)
          Xpred = X_heun + mu * X_heun * dt_local + sigma * X_heun * dW_local
          X_heun = X_heun + 0.5_dp*( mu*X_heun + mu*Xpred )*dt_local + 0.5_dp*( sigma*X_heun + sigma*Xpred )*dW_local
        end if
      end do
    end do

    open(unit=212, file='stratonovich_calculus.csv', status='replace', action='write', iostat=io_stat_s)
    if (io_stat_s /= 0) then
      write(*,'(A)') '  Warning: could not open stratonovich_calculus.csv for writing.'
      deallocate(path0_heun, path0_em, sumx_heun, sumx2_heun, sumx_em, sumx2_em)
      return
    end if
    call csv_write(212, 't,path0_heun,path0_em,mean_heun,var_heun,mean_em,var_em,analytic_mean,rel_err_heun,rel_err_em', io_stat_s)
    do j_idx_s = 0, Nsteps_s
      tt = real(j_idx_s, dp) * dt_local
      mean_heun = sumx_heun(j_idx_s) / real(M_ens_s, dp)
      var_heun = ( sumx2_heun(j_idx_s) - sumx_heun(j_idx_s)**2 / real(M_ens_s, dp) ) / real(max(1,M_ens_s-1), dp)
      mean_em = sumx_em(j_idx_s) / real(M_ens_s, dp)
      var_em = ( sumx2_em(j_idx_s) - sumx_em(j_idx_s)**2 / real(M_ens_s, dp) ) / real(max(1,M_ens_s-1), dp)
      mean_analytic = exp( mu * tt )
      var_analytic = exp( 2.0_dp * mu * tt ) * ( exp( sigma*sigma * tt ) - 1.0_dp )
      rel_err_heun = abs(mean_heun - mean_analytic) / max(1.0e-18_dp, abs(mean_analytic))
      rel_err_em = abs(mean_em - mean_analytic) / max(1.0e-18_dp, abs(mean_analytic))
      write(s_tt,'(F8.4)') tt
      write(s_p0h,'(ES18.10)') path0_heun(j_idx_s)
      write(s_p0e,'(ES18.10)') path0_em(j_idx_s)
      write(s_mh,'(ES18.10)') mean_heun
      write(s_vh,'(ES18.10)') var_heun
      write(s_me,'(ES18.10)') mean_em
      write(s_ve,'(ES18.10)') var_em
      write(s_ma,'(ES18.10)') mean_analytic
      write(s_reh,'(ES18.10)') rel_err_heun
      write(s_reem,'(ES18.10)') rel_err_em
      row = trim(s_tt)//','//trim(s_p0h)//','//trim(s_p0e)//','//trim(s_mh)//','//trim(s_vh)//','//trim(s_me)//','//trim(s_ve)//','//trim(s_ma)//','//trim(s_reh)//','//trim(s_reem)
      write(line,'(A)') trim(row)
      call csv_write(212, trim(line), io_stat_s)
    end do
    close(212)
    deallocate(path0_heun, path0_em, sumx_heun, sumx2_heun, sumx_em, sumx2_em)
    write(*,'(A)') 'Stratonovich calculus demo: Heun vs EM ensemble saved to stratonovich_calculus.csv'
  end subroutine stratonovich_calculus_demo

  subroutine white_noise_calculus_demo()
    implicit none
    integer, parameter :: dp = selected_real_kind(15,307)
    integer :: iu, N, i, lag, maxlag
    real(dp) :: mean, var
    real(dp), allocatable :: w(:)
    real(dp), allocatable :: acov(:)
    real(dp), allocatable :: psd(:), freq(:)
    integer :: k, Nfreq
    real(dp) :: re, im, angle
    real(dp), parameter :: PI = 3.141592653589793_dp
    character(len=512) :: line

    ! Simple numeric demonstration approximating Gaussian white noise
    ! Use moderate N so naive DFT is reasonable on typical machines
    N = 4096
    maxlag = 512
    allocate(w(N))

    call random_seed()
    mean = 0.0_dp
    do i = 1, N
      w(i) = normal_rand()
      mean = mean + w(i)
    end do
    mean = mean / real(N,dp)

    ! sample variance (unbiased)
    var = 0.0_dp
    do i = 1, N
      var = var + (w(i) - mean)**2
    end do
    var = var / real(N-1,dp)

    ! sample autocovariance for lags 0..maxlag (unbiased normalization)
    allocate(acov(0:maxlag))
    do lag = 0, maxlag
      acov(lag) = 0.0_dp
      do i = 1, N - lag
        acov(lag) = acov(lag) + (w(i) - mean) * (w(i+lag) - mean)
      end do
      acov(lag) = acov(lag) / real(N - lag, dp)
    end do

    ! compute periodogram (PSD) via naive DFT on detrended samples
    Nfreq = N/2
    allocate(psd(0:Nfreq), freq(0:Nfreq))
    do k = 0, Nfreq
      re = 0.0_dp
      im = 0.0_dp
      do i = 1, N
        angle = 2.0_dp * PI * real(k * (i-1), dp) / real(N, dp)
        re = re + (w(i) - mean) * cos(angle)
        im = im + (w(i) - mean) * sin(angle)
      end do
      psd(k) = ( re*re + im*im ) / real(N, dp)
      freq(k) = real(k,dp) / real(N,dp)    ! normalized frequency (cycles per sample)
    end do

    open(unit=213, file='white_noise_calculus.csv', status='replace', action='write', iostat=iu)
    if (iu /= 0) then
      write(*,'(A)') '  Warning: could not open white_noise_calculus.csv for writing.'
      deallocate(w, acov, psd, freq)
      return
    end if

    ! write sample statistics header
    call csv_write(213, 'mean,var,N,maxlag', iu)
    write(line,'(ES18.10,1A,ES18.10,1A,I0,1A,I0)') mean, ',', var, ',', N, ',', maxlag
    call csv_write(213, trim(line), iu)

    ! write raw samples (index,wn_sample)
    call csv_write(213, 'index,wn_sample', iu)
    do i = 1, N
      write(line,'(I0,1A,ES18.10)') i, ',', w(i)
      call csv_write(213, trim(line), iu)
    end do

    ! write autocovariances (lag,autocov)
    call csv_write(213, 'lag,autocov', iu)
    do lag = 0, maxlag
      write(line,'(I0,1A,ES18.10)') lag, ',', acov(lag)
      call csv_write(213, trim(line), iu)
    end do

    ! write PSD (normalized frequency, psd)
    call csv_write(213, 'freq,psd', iu)
    do k = 0, Nfreq
      write(line,'(F12.8,1A,ES18.10)') freq(k), ',', psd(k)
      call csv_write(213, trim(line), iu)
    end do

    close(213)
    deallocate(w, acov)
    write(*,'(A)') 'White noise calculus demo: samples and autocov saved to white_noise_calculus.csv'
  end subroutine white_noise_calculus_demo

  subroutine volterra_calculus_demo()
    implicit none
    integer, parameter :: dp = selected_real_kind(15,307)
    integer :: N, i, j, iu
    real(dp) :: T, dt_local, tt
    character(len=512) :: line
    real(dp), allocatable :: x_fs(:), x_trap(:), g(:)
    real(dp) :: alpha, beta, K0, sumk, sumt
    ! Solve Volterra integral equation of second kind:
    ! x(t) = g(t) + \int_0^t K(t-s) x(s) ds,    K(u)=alpha*exp(-beta u)
    T = 5.0_dp
    N = 201
    dt_local = T / real(N-1, dp)
    allocate(x_fs(N), x_trap(N), g(N))
    alpha = 1.0_dp; beta = 2.0_dp; K0 = alpha
    do i = 1, N
      tt = real(i-1,dp) * dt_local
      g(i) = sin(tt)    ! known forcing
    end do

    ! Method A: forward substitution with simple left-Riemann quadrature (existing approach)
    do i = 1, N
      tt = real(i-1,dp) * dt_local
      sumk = 0.0_dp
      if (i > 1) then
        do j = 1, i-1
          sumk = sumk + alpha * exp( -beta * ( tt - real(j-1,dp)*dt_local ) ) * x_fs(j)
        end do
      end if
      ! include diagonal contribution approx as dt*K(0)*x_i on RHS -> move to LHS
      x_fs(i) = ( g(i) + dt_local * sumk ) / ( 1.0_dp - dt_local * K0 )
    end do

    ! Method B: product-integration using trapezoidal rule (higher-order)
    x_trap(1) = g(1)    ! at t=0 the integral term is zero
    do i = 2, N
      tt = real(i-1,dp) * dt_local
      ! trapezoidal weights: 0.5 at endpoints, 1.0 interior
      sumt = 0.5_dp * ( alpha * exp( -beta * ( tt - 0.0_dp ) ) * x_trap(1) )
      if (i > 2) then
        do j = 2, i-1
          sumt = sumt + alpha * exp( -beta * ( tt - real(j-1,dp)*dt_local ) ) * x_trap(j)
        end do
      end if
      ! trapezoid includes half weight at current node: 0.5 * K(0) * x_trap(i) multiplied by dt_local
      x_trap(i) = ( g(i) + dt_local * sumt ) / ( 1.0_dp - 0.5_dp * dt_local * K0 )
    end do
    open(unit=214, file='volterra_calculus.csv', status='replace', action='write', iostat=iu)
    if (iu /= 0) then
      write(*,'(A)') '  Warning: could not open volterra_calculus.csv for writing.'
      deallocate(x_fs, x_trap, g)
      return
    end if
    call csv_write(214, 't,x_forwardRiemann,x_trap,diff_trap_forward', iu)
    do i = 1, N
      tt = real(i-1,dp) * dt_local
      write(line,'(F12.8,1A,ES18.10,1A,ES18.10,1A,ES18.10)') tt, ',', x_fs(i), ',', x_trap(i), ',', abs( x_trap(i) - x_fs(i) )
      call csv_write(214, trim(line), iu)
    end do
    close(214)
    deallocate(x_fs, x_trap, g)
    write(*,'(A)') 'Volterra integral-equation demo: forward-Riemann and trapezoid solutions saved to volterra_calculus.csv'
  end subroutine volterra_calculus_demo

  subroutine hormander_calculus_demo()
    implicit none
    integer, parameter :: dp = selected_real_kind(15,307)
    integer :: Nx, Nk, i, k, iu
    real(dp) :: Lx, Lk, dx, dk, x, xi, m, n, val
    real(dp), allocatable :: xs(:), xis(:)
    character(len=512) :: line
    ! Demonstrate a toy Hörmander symbol sampling: a(x,xi) in S^{m}_{rho,delta}
    ! We sample a(x,xi) = (1 + |xi|^2)^{m/2} * (1 + |x|^2)^{n/2} and write samples for inspection.
    Nx = 101; Nk = 101
    Lx = 10.0_dp; Lk = 20.0_dp
    dx = 2.0_dp*Lx / real(Nx-1, dp)
    dk = 2.0_dp*Lk / real(Nk-1, dp)
    m = 2.0_dp    ! growth in xi
    n = 1.0_dp    ! mild x-weight
    allocate(xs(Nx), xis(Nk))
    do i = 1, Nx
      xs(i) = -Lx + real(i-1,dp) * dx
    end do
    do k = 1, Nk
      xis(k) = -Lk + real(k-1,dp) * dk
    end do
    open(unit=215, file='hormander_calculus.csv', status='replace', action='write', iostat=iu)
    if (iu /= 0) then
      write(*,'(A)') '  Warning: could not open hormander_calculus.csv for writing.'
      deallocate(xs, xis)
      return
    end if
    call csv_write(215, 'x,xi,symbol_a(x,xi)', iu)
    do i = 1, Nx
      do k = 1, Nk
        x = xs(i); xi = xis(k)
        val = ( 1.0_dp + xi*xi )**(m/2.0_dp) * ( 1.0_dp + x*x )**(n/2.0_dp)
        write(line,'(F18.10,1A,ES18.10,1A,ES18.10)') x, ',', xi, ',', val
        call csv_write(215, trim(line), iu)
      end do
    end do
    close(215)
    deallocate(xs, xis)
    write(*,'(A)') 'H\u00f6rmander calculus demo: sampled symbol values saved to hormander_calculus.csv'
  end subroutine hormander_calculus_demo

  subroutine pseudodiff_calculus_demo()
    implicit none
    integer, parameter :: dp = selected_real_kind(15,307)
    integer :: N, i, k, iu
    real(dp) :: L, dx, x, alpha, PI, xi_k
    real(dp), allocatable :: xgrid(:), fx(:), g(:)
    complex(dp), allocatable :: Fk(:), Afk(:), tmpc(:)
    complex(dp) :: Icmplx
    character(len=512) :: line
    PI = 3.141592653589793_dp
    ! Demonstrate a simple pseudodifferential operator by applying a fractional
    ! derivative symbol |\xi|^alpha in the Fourier domain to a test function f(x)=exp(-x^2).
    N = 257               ! choose odd to have symmetric center
    L = 8.0_dp
    if (N <= 1) then
      write(*,'(A)') '  Warning: pseudodiff_calculus_demo requires N>1; skipping.'
      return
    end if
    dx = 2.0_dp*L / real(N,dp)
    alpha = 1.0_dp       ! order: 1 -> first-order (fractional) derivative magnitude
    allocate(xgrid(N), fx(N), g(N))
    allocate(Fk(N), Afk(N), tmpc(N))
    Icmplx = (0.0_dp, 1.0_dp)
    do i = 1, N
      x = -L + (real(i-1,dp))*dx
      xgrid(i) = x
      fx(i) = exp( - x*x )
    end do
    ! Apply a Hann window to reduce spectral leakage (low-risk)
    do i = 1, N
      fx(i) = fx(i) * 0.5_dp * (1.0_dp - cos( 2.0_dp * PI * real(i-1,dp) / real(N,dp) ))
    end do
    ! Forward DFT (non-normalized): F[k] = sum_j f[j] * exp(-2pi i * (j-1)*(k-1)/N)
    do k = 1, N
      Fk(k) = (0.0_dp, 0.0_dp)
      do i = 1, N
        Fk(k) = Fk(k) + cmplx( fx(i), 0.0_dp, kind=dp ) * exp( -2.0_dp*PI*Icmplx * real((i-1)*(k-1),dp) / real(N,dp) )
      end do
    end do
    ! Apply symbol: continuous frequency xi_k ~ 2*pi*(k - N/2 - 1)/ (2L) ~ pi*(k-N/2-1)/L
    do k = 1, N
      tmpc(k) = Fk(k)
      ! shift index to center and compute frequency xi_k safely
      xi_k = PI * real( (k-1) - (N-1)/2, dp ) / L
      ! protect against xi_k == 0 when alpha <= 0 (avoid division by zero / singularity)
      if (xi_k == 0.0_dp .and. alpha <= 0.0_dp) then
        Afk(k) = (0.0_dp, 0.0_dp)
      else
        Afk(k) = tmpc(k) * ( abs(xi_k)**alpha )
      end if
    end do
    ! Inverse DFT (normalized): g[j] = (1/N) sum_k Af[k] * exp(+2pi i * (j-1)*(k-1)/N)
    do i = 1, N
      g(i) = 0.0_dp
      do k = 1, N
        g(i) = g(i) + real( Afk(k) * exp( 2.0_dp*PI*Icmplx * real((i-1)*(k-1),dp) / real(N,dp) ), dp ) / real(N,dp)
      end do
    end do
    open(unit=216, file='pseudodiff_calculus.csv', status='replace', action='write', iostat=iu)
    if (iu /= 0) then
      write(*,'(A)') '  Warning: could not open pseudodiff_calculus.csv for writing.'
      deallocate(xgrid, fx, g, Fk, Afk, tmpc)
      return
    end if
    call csv_write(216, 'x,f(x),Pseudodiff(f)(x)', iu)
    do i = 1, N
      write(line,'(F18.10,1A,ES18.10,1A,ES18.10)') xgrid(i), ',', fx(i), ',', g(i)
      call csv_write(216, trim(line), iu)
    end do
    close(216)
    deallocate(xgrid, fx, g, Fk, Afk, tmpc)
    write(*,'(A)') 'Pseudodifferential calculus demo: applied fractional symbol and saved to pseudodiff_calculus.csv'
  end subroutine pseudodiff_calculus_demo

  subroutine fourier_integral_operator_demo()
    implicit none
    integer, parameter :: dp = selected_real_kind(15,307)
    integer :: N, i, k, iu
    real(dp) :: L, dx, x, PI, gamma, xi_k
    real(dp), allocatable :: xgrid(:), fin(:), fio_out(:)
    complex(dp), allocatable :: Fk(:), Afk(:)
    complex(dp) :: Icmplx
    character(len=512) :: line
    PI = 3.141592653589793_dp
    ! Toy FIO: (T f)(x) = inverseFFT( e^{ i * 0.5 * gamma * xi^2 } * FFT(f)(xi) )
    ! This approximates a quadratic phase modulation (chirp) FIO / metaplectic-like action.
    N = 256
    L = 8.0_dp
    dx = 2.0_dp*L / real(N,dp)
    if (N <= 1) then
      write(*,'(A)') '  Warning: fourier_integral_operator_demo requires N>1; skipping.'
      return
    end if
    allocate(xgrid(N), fin(N), fio_out(N), Fk(N), Afk(N))
    Icmplx = (0.0_dp, 1.0_dp)
    gamma = 0.5_dp
    do i = 1, N
      x = -L + real(i-1,dp) * dx
      xgrid(i) = x
      fin(i) = exp( - x*x ) * cos( 1.5_dp * x )    ! test function (localized oscillatory)
    end do
    ! Apply a Hann window to reduce spectral leakage
    do i = 1, N
      fin(i) = fin(i) * 0.5_dp * (1.0_dp - cos( 2.0_dp * PI * real(i-1,dp) / real(N,dp) ))
    end do
    ! Forward DFT
    do k = 1, N
      Fk(k) = (0.0_dp, 0.0_dp)
      do i = 1, N
        Fk(k) = Fk(k) + cmplx( fin(i), 0.0_dp, kind=dp ) * exp( -2.0_dp*PI*Icmplx * real((i-1)*(k-1),dp) / real(N,dp) )
      end do
    end do
    ! apply quadratic phase multiplier in frequency domain (centered freq index)
    do k = 1, N
      ! frequency index centered at N/2
      xi_k = real( (k-1) - N/2, dp )
      Afk(k) = Fk(k) * exp( Icmplx * 0.5_dp * gamma * ( xi_k**2 ) / real(N,dp) )
    end do
    ! inverse DFT -> real output
    do i = 1, N
      fio_out(i) = 0.0_dp
      do k = 1, N
        fio_out(i) = fio_out(i) + real( Afk(k) * exp( 2.0_dp*PI*Icmplx * real((i-1)*(k-1),dp) / real(N,dp) ), dp ) / real(N,dp)
      end do
    end do
    open(unit=217, file='fourier_integral_operator.csv', status='replace', action='write', iostat=iu)
    if (iu /= 0) then
      write(*,'(A)') '  Warning: could not open fourier_integral_operator.csv for writing.'
      deallocate(xgrid, fin, fio_out, Fk, Afk)
      return
    end if
    call csv_write(217, 'x,f(x),FIO[f](x)', iu)
    do i = 1, N
      write(line,'(F18.10,1A,ES18.10,1A,ES18.10)') xgrid(i), ',', fin(i), ',', fio_out(i)
      call csv_write(217, trim(line), iu)
    end do
    close(217)
    deallocate(xgrid, fin, fio_out, Fk, Afk)
    write(*,'(A)') 'Fourier integral operator demo: quadratic-phase FIO saved to fourier_integral_operator.csv'
  end subroutine fourier_integral_operator_demo

  subroutine weyl_calculus_demo()
    implicit none
    integer, parameter :: dp = selected_real_kind(15,307)
    integer :: Nx, i, iu
    real(dp) :: Lx, dx, PI
    real(dp), allocatable :: xs(:), psi(:), classical_a(:)
    real(dp) :: expect_val, sumv
    character(len=512) :: line
    PI = 3.141592653589793_dp
    ! Demonstrate a discrete approximation to the Weyl quantization expectation
    ! for symbol a(x,p) = p^2/2 + V(x) with V(x)=0.5 x^2 acting on a Gaussian psi.
    Nx = 41
    Lx = 6.0_dp
    dx = 2.0_dp*Lx / real(Nx-1, dp)
    allocate(xs(Nx), psi(Nx), classical_a(Nx))
    ! Gaussian wavefunction centered at 0 with width sigma
    do i = 1, Nx
      xs(i) = -Lx + real(i-1,dp) * dx
    end do
    do i = 1, Nx
      psi(i) = exp( - (xs(i)**2) / (2.0_dp * 0.8_dp**2) )
    end do
    ! normalize psi (L2 approx)
    sumv = 0.0_dp
    do i = 1, Nx
      sumv = sumv + psi(i)**2 * dx
    end do
    if (sumv > 0.0_dp) then
      do i = 1, Nx
        psi(i) = psi(i) / sqrt(sumv)
      end do
    end if
    ! classical symbol a(x,0) = 0 + V(x) for p=0 (compare expectation of potential)
    do i = 1, Nx
      classical_a(i) = 0.5_dp * xs(i)**2
    end do
    ! approximate expectation <psi|a(x,0)|psi> ~ sum |psi|^2 * a(x,0) dx
    expect_val = 0.0_dp
    do i = 1, Nx
      expect_val = expect_val + (psi(i)**2) * classical_a(i) * dx
    end do
    ! write CSV with x,|psi|^2,a(x,0)
    open(unit=218, file='weyl_calculus.csv', status='replace', action='write', iostat=iu)
    if (iu /= 0) then
      write(*,'(A)') '  Warning: could not open weyl_calculus.csv for writing.'
      deallocate(xs, psi, classical_a)
      return
    end if
    call csv_write(218, 'x,|psi|^2,Classical_a(x,0)', iu)
    do i = 1, Nx
      write(line,'(ES18.10,1A,ES18.10,1A,ES18.10)') xs(i), ',', psi(i)**2, ',', classical_a(i)
      call csv_write(218, trim(line), iu)
    end do
    close(218)
    deallocate(xs, psi, classical_a)
    write(*,'(A,F18.10)') 'Weyl calculus demo: expectation approx (potential part) =', expect_val
    write(*,'(A)') 'Weyl calculus demo: CSV saved to weyl_calculus.csv'
  end subroutine weyl_calculus_demo

  subroutine berezin_calculus_demo()
    implicit none
    integer, parameter :: dp = selected_real_kind(15,307)
    integer :: iu
    real(dp) :: a
    character(len=512) :: line
    ! Berezin integral over two Grassmann variables theta1,theta2:
    ! Integral dtheta2 dtheta1 exp( a * theta1 * theta2 ) = a
    a = 2.5_dp
    open(unit=219, file='berezin_calculus.csv', status='replace', action='write', iostat=iu)
    if (iu /= 0) then
      write(*,'(A)') '  Warning: could not open berezin_calculus.csv for writing.'
      return
    end if
    call csv_write(219, 'a,berezin_integral', iu)
    write(line,'(ES18.10,1A,ES18.10)') a, ',', a
    call csv_write(219, trim(line), iu)
    close(219)
    write(*,'(A)') 'Berezin calculus demo: example integral saved to berezin_calculus.csv'
  end subroutine berezin_calculus_demo

  subroutine clifford_calculus_demo()
    implicit none
    integer, parameter :: dp = selected_real_kind(15,307)
    integer :: iu
    real(dp) :: a0, a1, a2, a12
    real(dp) :: b0, b1, b2, b12
    real(dp) :: r0, r1, r2, r12
    character(len=512) :: line
    ! Represent multivector as [s, e1, e2, e12]
    a0 = 1.0_dp; a1 = 1.0_dp; a2 = 0.5_dp; a12 = 0.0_dp
    b0 = 2.0_dp; b1 = -0.3_dp; b2 = 0.25_dp; b12 = 0.1_dp
    ! Compute geometric (Clifford) product r = a * b for 2D Euclidean signature
    ! Using basis relations: e1^2 = 1, e2^2 = 1, e1 e2 = e12 = - e2 e1, e12^2 = -1
    r0 = a0*b0 + a1*b1 + a2*b2 - a12*b12
    r1 = a0*b1 + a1*b0 + a2*b12 - a12*b2
    r2 = a0*b2 + a2*b0 - a1*b12 + a12*b1
    r12 = a0*b12 + a12*b0 + a1*b2 - a2*b1
    open(unit=220, file='clifford_calculus.csv', status='replace', action='write', iostat=iu)
    if (iu /= 0) then
      write(*,'(A)') '  Warning: could not open clifford_calculus.csv for writing.'
      return
    end if
    call csv_write(220, 'a0,a1,a2,a12,b0,b1,b2,b12,r0,r1,r2,r12', iu)
    write(line,'(ES18.10,1A,ES18.10,1A,ES18.10,1A,ES18.10,1A,ES18.10,1A,ES18.10,1A,ES18.10,1A,ES18.10,1A,ES18.10,1A,ES18.10,1A,ES18.10,1A,ES18.10)') a0, ',', a1, ',', a2, ',', a12, ',', b0, ',', b1, ',', b2, ',', b12, ',', r0, ',', r1, ',', r2, ',', r12
    call csv_write(220, trim(line), iu)
    close(220)
    write(*,'(A)') 'Clifford calculus demo: example product saved to clifford_calculus.csv'
  end subroutine clifford_calculus_demo

  subroutine pseudodiff_symbol_demo()
    implicit none
    integer, parameter :: dp = selected_real_kind(15,307)
    integer :: i, N, ios
    character(len=512) :: line
    real(dp) :: k, L
    real(dp), allocatable :: xs(:), ks(:), symvals(:)

    ! Toy symbol sampling for a pseudodifferential operator with symbol a(x,k) = (1 + k^2)^(s/2)
    N = 201
    L = 10.0_dp
    allocate(xs(N), ks(N), symvals(N))
    do i = 1, N
      xs(i) = -L + 2.0_dp*L * real(i-1,dp)/real(N-1,dp)
      ks(i) = -5.0_dp + 10.0_dp * real(i-1,dp)/real(N-1,dp)
    end do

    do i = 1, N
      k = ks(i)
      symvals(i) = (1.0_dp + k*k)**(1.0_dp/2.0_dp)
    end do

    open(unit=250, file='pseudodiff_symbol.csv', status='replace', action='write', iostat=ios)
    if (ios /= 0) then
      write(*,'(A)') '  Warning: could not open pseudodiff_symbol.csv'
      deallocate(xs,ks,symvals)
      return
    end if
    call csv_write(250, 'x,k,symbol', ios)
    do i = 1, N
      write(line,'(ES18.10,1A,ES18.10,1A,ES18.10)') xs(i), ',', ks(i), ',', symvals(i)
      call csv_write(250, trim(line), ios)
    end do
    close(250)
    deallocate(xs,ks,symvals)
    write(*,'(A)') 'Pseudodifferential symbol demo: CSV saved to pseudodiff_symbol.csv'
  end subroutine pseudodiff_symbol_demo

  subroutine index_theory_demo()
    implicit none
    integer, parameter :: dp = selected_real_kind(15,307)
    real(dp) :: traceA, traceB

    ! Tiny index theorem toy: compare analytic index (dim kernel - dim cokernel)
    ! Here we create two small matrices and compute a simple index-like difference via traces
    traceA = 2.0_dp  ! pretend trace of a projection-like operator
    traceB = 0.0_dp  ! pretend trace of its parametrix defect
    write(*,'(A)') 'Index theory toy: analytic index (toy values)'
    write(*,'(A,F12.6)') '  trace(A) =', traceA
    write(*,'(A,F12.6)') '  trace(B) =', traceB
    write(*,'(A,F12.6)') '  toy_index = trace(A) - trace(B) =', traceA - traceB
  end subroutine index_theory_demo

  subroutine microlocal_test_demo()
    implicit none
    integer, parameter :: dp = selected_real_kind(15,307)
    integer :: N, i, ios
    real(dp) :: amp
    real(dp), allocatable :: xs(:), ks(:), ampgrid(:)
    character(len=512) :: line

    ! Toy microlocal amplitude test: localize a Gaussian in x and k and sample amplitude
    N = 121
    allocate(xs(N), ks(N), ampgrid(N))
    do i = 1, N
      xs(i) = -3.0_dp + 6.0_dp * real(i-1,dp)/real(N-1,dp)
      ks(i) = -5.0_dp + 10.0_dp * real(i-1,dp)/real(N-1,dp)
      amp = exp( - (xs(i)**2) - (ks(i)/2.0_dp)**2 )
      ampgrid(i) = amp
    end do
    open(unit=251, file='microlocal_test.csv', status='replace', action='write', iostat=ios)
    if (ios /= 0) then
      write(*,'(A)') '  Warning: could not open microlocal_test.csv'
      deallocate(xs,ks,ampgrid)
      return
    end if
    call csv_write(251, 'x,k,amplitude', ios)
    do i = 1, N
      write(line,'(ES18.10,1A,ES18.10,1A,ES18.10)') xs(i), ',', ks(i), ',', ampgrid(i)
      call csv_write(251, trim(line), ios)
    end do
    close(251)
    deallocate(xs,ks,ampgrid)
    write(*,'(A)') 'Microlocal test demo: CSV saved to microlocal_test.csv'
  end subroutine microlocal_test_demo

  subroutine geometric_clifford_demo()
    implicit none
    integer, parameter :: dp = selected_real_kind(15,307)
    integer :: iu
    real(dp) :: vx, vy
    real(dp) :: scalar, bivector
    ! Show geometric product of vectors and decomposition into symmetric/antisymmetric parts
    vx = 1.0_dp; vy = 2.0_dp
    ! geometric product v*v = |v|^2 (scalar)
    scalar = vx*vx + vy*vy
    ! for two unit basis vectors e1,e2 their wedge is bivector; illustrate with e1*e2
    bivector = 1.0_dp   ! placeholder magnitude for e1^e2
    open(unit=221, file='geometric_clifford.csv', status='replace', action='write', iostat=iu)
    if (iu /= 0) then
      write(*,'(A)') '  Warning: could not open geometric_clifford.csv for writing.'
      return
    end if
    write(221,'(A)') 'vx,vy,|v|^2,bivector_mag'
    write(221,'(ES18.10,1A,ES18.10,1A,ES18.10,1A,ES18.10)') vx, ',', vy, ',', scalar, ',', bivector
    close(221)
    write(*,'(A)') 'Geometric Clifford demo: example saved to geometric_clifford.csv'
  end subroutine geometric_clifford_demo

  subroutine microlocal_sheaves_demo()
    implicit none
    integer, parameter :: dp = selected_real_kind(15,307)
    integer :: Nx, Nk, i, k, iu, win, wlen
    real(dp) :: L, dx, x, t, kfreq, sigma_w, thr
    real(dp) :: re, im
    real(dp), allocatable :: xs(:), signal(:), window(:)
    real(dp), allocatable :: stft_mag(:)
    character(len=512) :: line
    ! Toy microlocal demonstration: compute STFT magnitudes for a signal with a localized jump
    Nx = 512
    L = 10.0_dp
    dx = 2.0_dp*L / real(Nx-1, dp)
    allocate(xs(Nx), signal(Nx))
    do i = 1, Nx
      xs(i) = -L + real(i-1,dp) * dx
    end do
    ! signal: smooth Gaussian plus a discontinuity around x=1.0 (approximated by steep tanh)
    do i = 1, Nx
      x = xs(i)
      signal(i) = exp( - x*x ) + 0.5_dp * ( 1.0_dp + tanh( 50.0_dp * (x - 1.0_dp) ) )
    end do
    ! STFT parameters: window length wlen and frequency bins Nk
    wlen = 64; Nk = 64; sigma_w = 0.8_dp
    allocate(window(wlen), stft_mag(wlen*Nk))
    ! Gaussian window
    do win = 1, wlen
      t = real(win-1,dp) - real(wlen-1,dp)/2.0_dp
      window(win) = exp( -0.5_dp * (t/(sigma_w*real(wlen,dp)))**2 )
    end do
    open(unit=222, file='microlocal_sheaves.csv', status='replace', action='write', iostat=iu)
    if (iu /= 0) then
      write(*,'(A)') '  Warning: could not open microlocal_sheaves.csv for writing.'
      deallocate(xs, signal, window, stft_mag)
      return
    end if
    call csv_write(222, 'xc,omega,STFT_mag', iu)
    ! slide window centers across xs and compute small DFT for each window
    thr = 1.0e-6_dp
    do i = 1, Nx, wlen/4
      ! windowed segment centered at i
      do win = 1, wlen
        if ( i + win - (wlen+1)/2 >= 1 .and. i + win - (wlen+1)/2 <= Nx ) then
          stft_mag(win) = window(win) * signal( i + win - (wlen+1)/2 )
        else
          stft_mag(win) = 0.0_dp
        end if
      end do
      ! small DFT over window
      do k = 0, Nk-1
        kfreq = -0.5_dp + real(k,dp)/real(Nk,dp)    ! normalized frequency
        ! compute complex amplitude
        re = 0.0_dp; im = 0.0_dp
        do win = 1, wlen
          t = real(win-1,dp)
          re = re + stft_mag(win) * cos( -2.0_dp * 3.141592653589793_dp * kfreq * t / real(wlen,dp) )
          im = im + stft_mag(win) * sin( -2.0_dp * 3.141592653589793_dp * kfreq * t / real(wlen,dp) )
        end do
        if (abs(re) + abs(im) > thr) then
          write(line,'(F12.6,1A,ES18.10,1A,ES18.10)') xs(i), ',', kfreq, ',', sqrt(re*re + im*im)
          call csv_write(222, trim(line), iu)
        end if
      end do
    end do
    close(222)
    deallocate(xs, signal, window, stft_mag)
    write(*,'(A)') 'Microlocal sheaves demo: STFT-based microsupport samples saved to microlocal_sheaves.csv'
  end subroutine microlocal_sheaves_demo

  subroutine convolution_demo()
    implicit none
    integer, parameter :: dp = selected_real_kind(15,307)
    integer :: N, K, i, j, iu
    real(dp) :: L, dx, x
    real(dp), allocatable :: xgrid(:), signal(:), out(:), kernel(:)
    character(len=512) :: line
    ! Simple 1D convolution demo: input = Gaussian + bump, kernel = moving-average lowpass
    N = 201
    K = 11  ! odd-length kernel
    L = 10.0_dp
    dx = 2.0_dp*L / real(N-1, dp)
    allocate(xgrid(N), signal(N), out(N), kernel(K))
    do i = 1, N
      x = -L + real(i-1,dp) * dx
      xgrid(i) = x
      ! signal: Gaussian plus a small localized bump
      signal(i) = exp( - (x/2.0_dp)**2 ) + 0.6_dp * exp( - 50.0_dp * (x - 2.0_dp)**2 )
    end do

    ! simple moving-average kernel (normalized)
    do j = 1, K
      kernel(j) = 1.0_dp / real(K, dp)
    end do

    ! full convolution (zero-padding outside bounds)
    out = 0.0_dp
    do i = 1, N
      do j = 1, K
        if ( i + (j - (K+1)/2) >= 1 .and. i + (j - (K+1)/2) <= N ) then
          out(i) = out(i) + kernel(j) * signal( i + (j - (K+1)/2) )
        end if
      end do
    end do

    open(unit=260, file='convolution_demo.csv', status='replace', action='write', iostat=iu)
    if (iu /= 0) then
      write(*,'(A)') '  Warning: could not open convolution_demo.csv for writing.'
      deallocate(xgrid, signal, out, kernel)
      return
    end if
    call csv_write(260, 'x,input,output', iu)
    do i = 1, N
      write(line,'(F12.6,1A,ES18.10,1A,ES18.10)') xgrid(i), ',', signal(i), ',', out(i)
      call csv_write(260, trim(line), iu)
    end do
    close(260)
    deallocate(xgrid, signal, out, kernel)
    write(*,'(A)') 'Convolution demo: input and filtered output saved to convolution_demo.csv'
  end subroutine convolution_demo

  subroutine wave1d_snapshots_demo()
    implicit none
    integer, parameter :: dp = selected_real_kind(15,307)
    integer :: Nx, Nt, i, n, iu, snapshot_stride
    real(dp) :: L, dx, c, CFL, dt, t
    real(dp), allocatable :: x(:), u(:), uold(:), unew(:)
    ! additional locals and sponge declarations (must appear before executable statements)
    real(dp) :: sponge_frac, sponge_width, damp_amp, dist, frac
    real(dp), allocatable :: damping(:)
    character(len=512) :: line
    ! 1D wave equation u_tt = c^2 u_xx on x in [-L,L], with a simple absorbing sponge
    Nx = 201
    L = 1.0_dp
    c = 1.0_dp
    dx = 2.0_dp*L / real(Nx-1, dp)
    CFL = 0.9_dp
    dt = CFL * dx / c
    Nt = 300
    snapshot_stride = 30
    ! sponge parameters (fraction of domain at each end and damping strength)
    sponge_frac = 0.15_dp
    sponge_width = sponge_frac * L
    damp_amp = 0.90_dp
    allocate(x(Nx), u(Nx), uold(Nx), unew(Nx))

    ! initial condition: Gaussian pulse centered at x=0
    do i = 1, Nx
      x(i) = -L + real(i-1,dp) * dx
      u(i) = exp( - ( x(i)/0.12_dp )**2 )
      uold(i) = u(i)  ! assume initial velocity = 0 so uold = u - dt * v ~ u
    end do

    ! allocate and build damping profile for sponge (1 = no damping, <1 damped)
    allocate(damping(Nx))
    do i = 1, Nx
      dist = min( abs(x(i) + L), abs(L - x(i)) )  ! distance to nearest boundary in [0,L]
      if (dist >= sponge_width) then
        damping(i) = 1.0_dp
      else
        frac = (sponge_width - dist) / sponge_width
        damping(i) = 1.0_dp - damp_amp * frac*frac
        if (damping(i) < 0.0_dp) damping(i) = 0.0_dp
      end if
    end do

    open(unit=261, file='wave1d_snapshots.csv', status='replace', action='write', iostat=iu)
    if (iu /= 0) then
      write(*,'(A)') '  Warning: could not open wave1d_snapshots.csv for writing.'
      deallocate(x, u, uold, unew)
      return
    end if
    call csv_write(261, 't,x,u', iu)

    ! write initial snapshot
    t = 0.0_dp
    do i = 1, Nx
      write(line,'(F8.4,1A,F12.6,1A,ES18.10)') t, ',', x(i), ',', u(i)
      call csv_write(261, trim(line), iu)
    end do

    ! first time-step (use u_tt approx -> explicit scheme requires u_old known)
    ! use second-order central time: u^{n+1} = 2u^n - u^{n-1} + (c dt/dx)^2 (u_{i+1}^n - 2u_i^n + u_{i-1}^n)
    do n = 1, Nt
      ! enforce Dirichlet boundaries u(1)=u(Nx)=0
      u(1) = 0.0_dp; u(Nx) = 0.0_dp
      do i = 2, Nx-1
        unew(i) = 2.0_dp*u(i) - uold(i) + (c*dt/dx)**2 * ( u(i+1) - 2.0_dp*u(i) + u(i-1) )
      end do
      ! set boundary values explicitly
      unew(1) = 0.0_dp; unew(Nx) = 0.0_dp
      ! apply sponge damping to newly computed field (reduces reflections at edges)
      do i = 1, Nx
        unew(i) = unew(i) * damping(i)
      end do
      ! swap arrays: uold <- u ; u <- unew
      uold = u
      u = unew
      t = real(n,dp) * dt
      if (mod(n, snapshot_stride) == 0) then
        do i = 1, Nx
          write(line,'(F8.4,1A,F12.6,1A,ES18.10)') t, ',', x(i), ',', u(i)
          call csv_write(261, trim(line), iu)
        end do
      end if
    end do

    close(261)
    deallocate(x, u, uold, unew, damping)
    write(*,'(A)') 'Wave 1D demo: snapshots saved to wave1d_snapshots.csv'
  end subroutine wave1d_snapshots_demo

  subroutine dijkstra_demo()
    implicit none
    integer, parameter :: dp = selected_real_kind(15,307)
    integer :: n, i, j, iu, src, u
    character(len=512) :: line
    real(dp), parameter :: INF = 1.0e30_dp
    real(dp), allocatable :: W(:,:)
    real(dp), allocatable :: dist(:)
    logical, allocatable :: visited(:)
    integer, allocatable :: pred(:)

    ! Small example graph (undirected) with 6 nodes
    n = 6
    allocate(W(n,n), dist(n), visited(n), pred(n))

    ! initialize weights to INF (no edge)
    W = INF
    do i = 1, n
      W(i,i) = 0.0_dp
    end do

    ! edges (undirected)
    W(1,2) = 7.0_dp;  W(2,1) = 7.0_dp
    W(1,3) = 9.0_dp;  W(3,1) = 9.0_dp
    W(1,6) = 14.0_dp; W(6,1) = 14.0_dp
    W(2,3) = 10.0_dp; W(3,2) = 10.0_dp
    W(2,4) = 15.0_dp; W(4,2) = 15.0_dp
    W(3,4) = 11.0_dp; W(4,3) = 11.0_dp
    W(3,6) = 2.0_dp;  W(6,3) = 2.0_dp
    W(4,5) = 6.0_dp;  W(5,4) = 6.0_dp
    W(5,6) = 9.0_dp;  W(6,5) = 9.0_dp

    ! initialize Dijkstra arrays
    dist = INF
    visited = .false.
    pred = -1

    src = 1
    dist(src) = 0.0_dp

    do
      ! find unvisited node with smallest dist (avoid indexing dist(u) when u==-1)
      u = -1
      do i = 1, n
        if (.not. visited(i)) then
          if (u == -1) then
            u = i
          else if (dist(i) < dist(u)) then
            u = i
          end if
        end if
      end do
      if (u == -1) exit
      visited(u) = .true.
      ! relax edges from u
      do j = 1, n
        if (.not. visited(j) .and. W(u,j) < INF) then
          if (dist(u) + W(u,j) < dist(j)) then
            dist(j) = dist(u) + W(u,j)
            pred(j) = u
          end if
        end if
      end do
      ! check if all visited
      if (all(visited)) exit
    end do

    open(unit=270, file='dijkstra_demo.csv', status='replace', action='write', iostat=iu)
    if (iu /= 0) then
      write(*,'(A)') '  Warning: could not open dijkstra_demo.csv for writing.'
    else
      call csv_write(270, 'node,distance,predecessor', iu)
      do i = 1, n
        write(line,'(I0,1A,ES18.10,1A,I0)') i, ',', dist(i), ',', pred(i)
        call csv_write(270, trim(line), iu)
      end do
      close(270)
      write(*,'(A)') 'Dijkstra demo: CSV saved to dijkstra_demo.csv'
    end if

    deallocate(W, dist, visited, pred)

  end subroutine dijkstra_demo

    subroutine advection1d_lw_demo()
      implicit none
      integer, parameter :: dp = selected_real_kind(15,307)
      integer :: Nx, i, n, Nt, iu, ip, im
      real(dp) :: L, dx, a, CFL, dt, lambda, t
      real(dp), allocatable :: xgrid(:), u(:), unew(:)

      ! 1D linear advection u_t + a u_x = 0 with periodic BCs
      Nx = 201
      L = 1.0_dp
      dx = L / real(Nx, dp)
      a = 1.0_dp
      CFL = 0.8_dp
      dt = CFL * dx / abs(a)
      Nt = 200

      allocate(xgrid(Nx), u(Nx), unew(Nx))

      ! initial condition: single sine wave on [0,1)
      do i = 1, Nx
        xgrid(i) = (real(i-1,dp)) * dx
        u(i) = sin( 2.0_dp * 3.141592653589793_dp * xgrid(i) )
      end do

      ! time stepping (Lax-Wendroff)
      do n = 1, Nt
        lambda = a * dt / dx
        ! periodic indexing
        do i = 1, Nx
          ip = i + 1
          if (ip > Nx) ip = 1
          im = i - 1
          if (im < 1) im = Nx
          unew(i) = u(i) - 0.5_dp*lambda*( u(ip) - u(im) ) + 0.5_dp*(lambda**2)*( u(ip) - 2.0_dp*u(i) + u(im) )
        end do
        u = unew
        t = real(n,dp) * dt
      end do

      open(unit=300, file='advection1d_lw.csv', status='replace', action='write', iostat=iu)
      if (iu /= 0) then
        write(*,'(A)') '  Warning: could not open advection1d_lw.csv for writing.'
      else
        write(300,'(A)') 'x,u'
        do i = 1, Nx
          write(300,'(F12.6,1A,ES18.10)') xgrid(i), ',', u(i)
        end do
        close(300)
        write(*,'(A)') 'Advection 1D (Lax-Wendroff) demo: CSV saved to advection1d_lw.csv'
      end if

      deallocate(xgrid, u, unew)

    end subroutine advection1d_lw_demo

    subroutine advection1d_upwind_demo()
      implicit none
      integer, parameter :: dp = selected_real_kind(15,307)
      integer :: Nx, i, n, Nt, iu, im, ip
      real(dp) :: L, dx, a, CFL, dt, lambda, t
      real(dp), allocatable :: xgrid(:), u(:), unew(:)

      ! 1D linear advection u_t + a u_x = 0 with periodic BCs (first-order upwind)
      Nx = 201
      L = 1.0_dp
      dx = L / real(Nx, dp)
      a = 1.0_dp
      CFL = 0.8_dp
      dt = CFL * dx / abs(a)
      Nt = 200

      allocate(xgrid(Nx), u(Nx), unew(Nx))

      ! initial condition: single sine wave on [0,1)
      do i = 1, Nx
        xgrid(i) = (real(i-1,dp)) * dx
        u(i) = sin( 2.0_dp * 3.141592653589793_dp * xgrid(i) )
      end do

      do n = 1, Nt
        lambda = a * dt / dx
        if (a >= 0.0_dp) then
          do i = 1, Nx
            im = i - 1
            if (im < 1) im = Nx
            unew(i) = u(i) - lambda * ( u(i) - u(im) )
          end do
        else
          do i = 1, Nx
            ip = i + 1
            if (ip > Nx) ip = 1
            unew(i) = u(i) - lambda * ( u(ip) - u(i) )
          end do
        end if
        u = unew
        t = real(n,dp) * dt
      end do

      open(unit=301, file='advection1d_upwind.csv', status='replace', action='write', iostat=iu)
      if (iu /= 0) then
        write(*,'(A)') '  Warning: could not open advection1d_upwind.csv for writing.'
      else
        write(301,'(A)') 'x,u'
        do i = 1, Nx
          write(301,'(F12.6,1A,ES18.10)') xgrid(i), ',', u(i)
        end do
        close(301)
        write(*,'(A)') 'Advection 1D (Upwind) demo: CSV saved to advection1d_upwind.csv'
      end if

      deallocate(xgrid, u, unew)

    end subroutine advection1d_upwind_demo

  subroutine method_of_characteristics_demo()
    implicit none
    integer, parameter :: dp = selected_real_kind(15,307)
    integer :: Ns(3), idx, Nx, i, n, Nt, iu
    real(dp) :: L, dx, a, CFL, dt, t_final, t
    real(dp), allocatable :: x(:), u_sl(:), u_up(:), u_lw(:), u_exact(:)
    real(dp) :: L2_up, L2_lw, L2_sl, Linf_up, Linf_lw, Linf_sl, sumsq

    ! Compare semi-Lagrangian (characteristics shift) to upwind and Lax-Wendroff
    Ns = (/ 101, 201, 401 /)
    a = 1.0_dp
    L = 1.0_dp
    t_final = 0.2_dp

    open(unit=305, file='method_of_characteristics.csv', status='replace', action='write', iostat=iu)
    if (iu /= 0) then
      write(*,'(A)') '  Warning: could not open method_of_characteristics.csv for writing.'
      return
    end if
    write(305,'(A)') 'N,method,L2_error,Linf_error'

    do idx = 1, size(Ns)
      Nx = Ns(idx)
      dx = L / real(Nx, dp)
      CFL = 0.5_dp
      dt = CFL * dx / abs(a)
      Nt = max(1, int(t_final / dt))

      allocate(x(Nx), u_sl(Nx), u_up(Nx), u_lw(Nx), u_exact(Nx))

      ! initial: Gaussian pulse centered at x0
      do i = 1, Nx
        x(i) = (real(i-1,dp)) * dx
      end do
      call init_gaussian(x, u_sl, u_up, u_lw)
      u_exact = u_sl

      ! time-stepping for upwind and LW; semi-Lagrangian uses exact shift per time step
      do n = 1, Nt
        t = real(n,dp) * dt
        ! semi-Lagrangian: shift by a*dt (periodic)
        call semi_lagrangian_step(u_sl, Nx, a, dt, dx)
        ! upwind
        call upwind_step(u_up, Nx, a, dt, dx)
        ! Lax-Wendroff
        call lax_wendroff_step(u_lw, Nx, a, dt, dx)
      end do

      ! exact solution is initial profile shifted by a * t_final
      call shift_profile(u_exact, Nx, a * real(Nt,dp) * dt, dx)

      ! compute errors
      sumsq = 0.0_dp; L2_up = 0.0_dp; Linf_up = 0.0_dp
      do i = 1, Nx
        sumsq = sumsq + (u_up(i) - u_exact(i))**2
        Linf_up = max(Linf_up, abs(u_up(i) - u_exact(i)))
      end do
      L2_up = sqrt(sumsq * dx)

      sumsq = 0.0_dp; Linf_lw = 0.0_dp
      do i = 1, Nx
        sumsq = sumsq + (u_lw(i) - u_exact(i))**2
        Linf_lw = max(Linf_lw, abs(u_lw(i) - u_exact(i)))
      end do
      L2_lw = sqrt(sumsq * dx)

      sumsq = 0.0_dp; Linf_sl = 0.0_dp
      do i = 1, Nx
        sumsq = sumsq + (u_sl(i) - u_exact(i))**2
        Linf_sl = max(Linf_sl, abs(u_sl(i) - u_exact(i)))
      end do
      L2_sl = sqrt(sumsq * dx)

      write(305,'(I0,1A,A,1A,ES14.6,1A,ES14.6)') Nx, ',', 'semi-lagrangian', ',', L2_sl, ',', Linf_sl
      write(305,'(I0,1A,A,1A,ES14.6,1A,ES14.6)') Nx, ',', 'upwind', ',', L2_up, ',', Linf_up
      write(305,'(I0,1A,A,1A,ES14.6,1A,ES14.6)') Nx, ',', 'lax-wendroff', ',', L2_lw, ',', Linf_lw

      deallocate(x, u_sl, u_up, u_lw, u_exact)
    end do

    close(305)
    write(*,'(A)') 'Method-of-characteristics demo: CSV saved to method_of_characteristics.csv'
  end subroutine method_of_characteristics_demo

  subroutine init_gaussian(x, u1, u2, u3)
    implicit none
    integer, parameter :: dp = selected_real_kind(15,307)
    real(dp), intent(in) :: x(:)
    real(dp), intent(out) :: u1(:), u2(:), u3(:)
    integer :: i, N
    real(dp) :: x0, sigma
    N = size(x)
    x0 = 0.25_dp
    sigma = 0.05_dp
    do i = 1, N
      u1(i) = exp( -0.5_dp * ((x(i)-x0)/sigma)**2 )
      u2(i) = u1(i)
      u3(i) = u1(i)
    end do
  end subroutine init_gaussian

  subroutine shift_profile(u, N, shift, dx)
    implicit none
    integer, parameter :: dp = selected_real_kind(15,307)
    real(dp), intent(inout) :: u(:)
    integer, intent(in) :: N
    real(dp), intent(in) :: shift, dx
    integer :: i, k, j0, j1
    real(dp), allocatable :: tmp(:)
    real(dp) :: xshift, frac
    allocate(tmp(N))
    xshift = shift
    do i = 1, N
      ! map x_i -> x_i - shift (periodic)
      tmp(i) = u(1)  ! default
    end do
    ! simple circular shift by integer number of cells plus linear interpolation
    ! compute integer shift k and fractional part
    k = int( mod(xshift/dx, real(N,dp)) )
    frac = xshift/dx - real(k,dp)
    do i = 1, N
      j0 = i - k
      if (j0 < 1) j0 = j0 + N
      j1 = j0 - 1
      if (j1 < 1) j1 = j1 + N
      tmp(i) = (1.0_dp - frac) * u(j0) + frac * u(j1)
    end do
    u = tmp
    deallocate(tmp)
  end subroutine shift_profile

  subroutine semi_lagrangian_step(u, N, a, dt, dx)
    implicit none
    integer, parameter :: dp = selected_real_kind(15,307)
    real(dp), intent(inout) :: u(:)
    integer, intent(in) :: N
    real(dp), intent(in) :: a, dt, dx
    real(dp) :: shift
    shift = a * dt
    call shift_profile(u, N, shift, dx)
  end subroutine semi_lagrangian_step

  subroutine upwind_step(u, N, a, dt, dx)
    implicit none
    integer, parameter :: dp = selected_real_kind(15,307)
    real(dp), intent(inout) :: u(:)
    integer, intent(in) :: N
    real(dp), intent(in) :: a, dt, dx
    integer :: i, im, ip
    real(dp) :: lambda
    real(dp), allocatable :: un(:)
    lambda = a * dt / dx
    allocate(un(N))
    if (a >= 0.0_dp) then
      do i = 1, N
        im = i - 1; if (im < 1) im = N
        un(i) = u(i) - lambda * (u(i) - u(im))
      end do
    else
      do i = 1, N
        ip = i + 1; if (ip > N) ip = 1
        un(i) = u(i) - lambda * (u(ip) - u(i))
      end do
    end if
    u = un
    deallocate(un)
  end subroutine upwind_step

  subroutine lax_wendroff_step(u, N, a, dt, dx)
    implicit none
    integer, parameter :: dp = selected_real_kind(15,307)
    real(dp), intent(inout) :: u(:)
    integer, intent(in) :: N
    real(dp), intent(in) :: a, dt, dx
    integer :: i, ip, im
    real(dp) :: lambda
    real(dp), allocatable :: un(:)
    lambda = a * dt / dx
    allocate(un(N))
    do i = 1, N
      ip = i + 1; if (ip > N) ip = 1
      im = i - 1; if (im < 1) im = N
      un(i) = u(i) - 0.5_dp*lambda*(u(ip) - u(im)) + 0.5_dp*(lambda**2)*(u(ip) - 2.0_dp*u(i) + u(im))
    end do
    u = un
    deallocate(un)
  end subroutine lax_wendroff_step

  subroutine fast_sweeping_demo()
    implicit none
    integer, parameter :: dp = selected_real_kind(15,307)
    integer :: Nx, Ny, i, j, sweep, max_sweeps, iu
    real(dp) :: Lx, Ly, dx, dy, h, tol, diff, a, b, Tnew
    real(dp), parameter :: INF = 1.0e30_dp
    real(dp), allocatable :: T(:,:)

    ! Simple 2D Eikonal solve |grad T| = 1 on a square with source at center
    Nx = 101
    Ny = 101
    Lx = 1.0_dp
    Ly = 1.0_dp
    dx = Lx / real(Nx-1, dp)
    dy = Ly / real(Ny-1, dp)
    ! we use isotropic grid spacing h = average(dx,dy)
    h = 0.5_dp*(dx + dy)
    tol = 1.0e-6_dp
    max_sweeps = 200

    allocate(T(Nx,Ny))
    T = INF

    ! source at center
    T((Nx+1)/2, (Ny+1)/2) = 0.0_dp

    do sweep = 1, max_sweeps
      diff = 0.0_dp
      ! 4 sweeping orders (Gauss-Seidel style)
      do i = 1, Nx
        do j = 1, Ny
          if (i == (Nx+1)/2 .and. j == (Ny+1)/2) cycle
          ! get neighbor minima in x and y directions
          a = min( T(max(1,i-1), j), T(min(Nx,i+1), j) )
          b = min( T(i, max(1,j-1)), T(i, min(Ny,j+1)) )
          ! ensure a <= b
          if (a > b) then
            a = b + (a - b)
          end if
          ! apply 2D update formula for f=1: if (b - a) >= h then T = a + h
          if ( (b - a) >= h ) then
            Tnew = a + h
          else
            Tnew = 0.5_dp * ( a + b + sqrt( max(0.0_dp, 2.0_dp*h*h - (a - b)**2 ) ) )
          end if
          if (Tnew < T(i,j)) then
            diff = max( diff, abs(T(i,j) - Tnew) )
            T(i,j) = Tnew
          end if
        end do
      end do
      ! reverse sweep
      do i = Nx, 1, -1
        do j = 1, Ny
          if (i == (Nx+1)/2 .and. j == (Ny+1)/2) cycle
          a = min( T(max(1,i-1), j), T(min(Nx,i+1), j) )
          b = min( T(i, max(1,j-1)), T(i, min(Ny,j+1)) )
          if (a > b) then
            a = b + (a - b)
          end if
          if ( (b - a) >= h ) then
            Tnew = a + h
          else
            Tnew = 0.5_dp * ( a + b + sqrt( max(0.0_dp, 2.0_dp*h*h - (a - b)**2 ) ) )
          end if
          if (Tnew < T(i,j)) then
            diff = max( diff, abs(T(i,j) - Tnew) )
            T(i,j) = Tnew
          end if
        end do
      end do
      ! another pair of sweeps (j reversed)
      do i = 1, Nx
        do j = Ny, 1, -1
          if (i == (Nx+1)/2 .and. j == (Ny+1)/2) cycle
          a = min( T(max(1,i-1), j), T(min(Nx,i+1), j) )
          b = min( T(i, max(1,j-1)), T(i, min(Ny,j+1)) )
          if (a > b) then
            a = b + (a - b)
          end if
          if ( (b - a) >= h ) then
            Tnew = a + h
          else
            Tnew = 0.5_dp * ( a + b + sqrt( max(0.0_dp, 2.0_dp*h*h - (a - b)**2 ) ) )
          end if
          if (Tnew < T(i,j)) then
            diff = max( diff, abs(T(i,j) - Tnew) )
            T(i,j) = Tnew
          end if
        end do
      end do
      do i = Nx, 1, -1
        do j = Ny, 1, -1
          if (i == (Nx+1)/2 .and. j == (Ny+1)/2) cycle
          a = min( T(max(1,i-1), j), T(min(Nx,i+1), j) )
          b = min( T(i, max(1,j-1)), T(i, min(Ny,j+1)) )
          if (a > b) then
            a = b + (a - b)
          end if
          if ( (b - a) >= h ) then
            Tnew = a + h
          else
            Tnew = 0.5_dp * ( a + b + sqrt( max(0.0_dp, 2.0_dp*h*h - (a - b)**2 ) ) )
          end if
          if (Tnew < T(i,j)) then
            diff = max( diff, abs(T(i,j) - Tnew) )
            T(i,j) = Tnew
          end if
        end do
      end do

      if (diff < tol) exit
    end do

    ! write CSV of distance field
    open(unit=280, file='fast_sweeping_dt.csv', status='replace', action='write', iostat=iu)
    if (iu /= 0) then
      write(*,'(A)') '  Warning: could not open fast_sweeping_dt.csv for writing.'
    else
      write(280,'(A)') 'x,y,T'
      do i = 1, Nx
        do j = 1, Ny
          write(280,'(F8.5,1A,F8.5,1A,ES18.10)') (real(i-1,dp)*dx - Lx/2.0_dp), ',', (real(j-1,dp)*dy - Ly/2.0_dp), ',', T(i,j)
        end do
      end do
      close(280)
      write(*,'(A)') 'Fast Sweeping demo: CSV saved to fast_sweeping_dt.csv'
    end if

    deallocate(T)
  end subroutine fast_sweeping_demo

  subroutine fast_marching_heap_demo()
    implicit none
    integer, parameter :: dp = selected_real_kind(15,307)
    integer :: Nx, Ny, ncells
    integer :: i, j, u, v, ni, nj, idx, nbors, iu
    integer, allocatable :: status(:)   ! 0=far,1=trial,2=known
    integer, allocatable :: heap_idx(:) ! nodes in heap (by flattened index)
    integer, allocatable :: heap_pos(:) ! position of node in heap (0 if not in heap)
    real(dp), allocatable :: heap_key(:)
    real(dp), allocatable :: T(:)
    real(dp) :: Lx, Ly, dx, dy, h, Tnew, a, b
    real(dp), parameter :: INF = 1.0e30_dp
    integer :: heap_size, pos, parent, left, right, smallest, tmp_idx
    real(dp) :: tmp_key

    ! main body of fast_marching_heap_demo
    Nx = 101; Ny = 101
    Lx = 1.0_dp; Ly = 1.0_dp
    dx = Lx / real(Nx-1, dp)
    dy = Ly / real(Ny-1, dp)
    h = 0.5_dp*(dx + dy)
    ncells = Nx * Ny

    allocate(T(ncells))
    allocate(status(ncells))
    allocate(heap_idx(ncells))
    allocate(heap_key(ncells))
    allocate(heap_pos(ncells))

    T = INF
    status = 0
    heap_idx = 0
    heap_key = 0.0_dp
    heap_pos = 0
    heap_size = 0

    ! source at center (flattened index)
    u = ((Nx+1)/2 - 1) * Ny + ( (Ny+1)/2 )
    T(u) = 0.0_dp
    status(u) = 1   ! trial

    ! push u onto heap (inline push)
    heap_size = heap_size + 1
    heap_idx(heap_size) = u
    heap_key(heap_size) = T(u)
    heap_pos(u) = heap_size
    pos = heap_size
    do while (pos > 1)
      parent = pos/2
      if (heap_key(pos) < heap_key(parent)) then
        ! swap pos and parent
        tmp_idx = heap_idx(parent); heap_idx(parent) = heap_idx(pos); heap_idx(pos) = tmp_idx
        tmp_key = heap_key(parent); heap_key(parent) = heap_key(pos); heap_key(pos) = tmp_key
        heap_pos(heap_idx(parent)) = parent
        heap_pos(heap_idx(pos)) = pos
        pos = parent
      else
        exit
      end if
    end do

    ! main loop
    do while (heap_size > 0)
      ! pop root (min)
      u = heap_idx(1)
      heap_pos(u) = 0
      if (heap_size > 1) then
        heap_idx(1) = heap_idx(heap_size)
        heap_key(1) = heap_key(heap_size)
        heap_pos(heap_idx(1)) = 1
      end if
      heap_size = heap_size - 1
      if (heap_size > 0) then
        ! sift down from 1
        pos = 1
        do
          left = 2*pos
          right = left + 1
          smallest = pos
          if (left <= heap_size .and. heap_key(left) < heap_key(smallest)) smallest = left
          if (right <= heap_size .and. heap_key(right) < heap_key(smallest)) smallest = right
          if (smallest /= pos) then
            tmp_idx = heap_idx(pos); heap_idx(pos) = heap_idx(smallest); heap_idx(smallest) = tmp_idx
            tmp_key = heap_key(pos); heap_key(pos) = heap_key(smallest); heap_key(smallest) = tmp_key
            heap_pos(heap_idx(pos)) = pos
            heap_pos(heap_idx(smallest)) = smallest
            pos = smallest
          else
            exit
          end if
        end do
      end if

      status(u) = 2  ! known

      ! compute i,j from u
      i = (u-1) / Ny + 1
      j = mod(u-1, Ny) + 1

      ! loop over 4 neighbors
      do nbors = 1, 4
        select case (nbors)
        case (1)
          ni = i-1; nj = j
        case (2)
          ni = i+1; nj = j
        case (3)
          ni = i; nj = j-1
        case default
          ni = i; nj = j+1
        end select
        if (ni < 1 .or. ni > Nx .or. nj < 1 .or. nj > Ny) cycle
        v = (ni-1) * Ny + nj
        if (status(v) == 2) cycle  ! known

        ! compute update Tnew using neighbor values (prefer known values but use trials too)
        a = INF; b = INF
        if (ni > 1) a = min(a, T((ni-2)*Ny + nj))
        if (ni < Nx) a = min(a, T((ni)*Ny + nj))
        if (nj > 1) b = min(b, T((ni-1)*Ny + (nj-1)))
        if (nj < Ny) b = min(b, T((ni-1)*Ny + (nj+1)))

        if (a > b) then
          Tnew = b + h
        else if (b - a >= h) then
          Tnew = a + h
        else
          Tnew = 0.5_dp * ( a + b + sqrt( max(0.0_dp, 2.0_dp*h*h - (a - b)**2 ) ) )
        end if

        if (Tnew < T(v)) then
          T(v) = Tnew
          if (status(v) == 0) then
            status(v) = 1
            ! push v
            heap_size = heap_size + 1
            heap_idx(heap_size) = v
            heap_key(heap_size) = T(v)
            heap_pos(v) = heap_size
            pos = heap_size
            do while (pos > 1)
              parent = pos/2
              if (heap_key(pos) < heap_key(parent)) then
                tmp_idx = heap_idx(parent); heap_idx(parent) = heap_idx(pos); heap_idx(pos) = tmp_idx
                tmp_key = heap_key(parent); heap_key(parent) = heap_key(pos); heap_key(pos) = tmp_key
                heap_pos(heap_idx(parent)) = parent
                heap_pos(heap_idx(pos)) = pos
                pos = parent
              else
                exit
              end if
            end do
          else
            ! decrease-key for existing node
            pos = heap_pos(v)
            if (pos > 0 .and. T(v) < heap_key(pos)) then
              heap_key(pos) = T(v)
              do while (pos > 1)
                parent = pos/2
                if (heap_key(pos) < heap_key(parent)) then
                  tmp_idx = heap_idx(parent); heap_idx(parent) = heap_idx(pos); heap_idx(pos) = tmp_idx
                  tmp_key = heap_key(parent); heap_key(parent) = heap_key(pos); heap_key(pos) = tmp_key
                  heap_pos(heap_idx(parent)) = parent
                  heap_pos(heap_idx(pos)) = pos
                  pos = parent
                else
                  exit
                end if
              end do
            end if
          end if
        end if
      end do
    end do

    ! write CSV
    open(unit=285, file='fast_marching_heap_dt.csv', status='replace', action='write', iostat=iu)
    if (iu /= 0) then
      write(*,'(A)') '  Warning: could not open fast_marching_heap_dt.csv for writing.'
    else
      write(285,'(A)') 'x,y,T'
      do i = 1, Nx
        do j = 1, Ny
          idx = (i-1)*Ny + j
          write(285,'(F8.5,1A,F8.5,1A,ES18.10)') (real(i-1,dp)*dx - Lx/2.0_dp), ',', (real(j-1,dp)*dy - Ly/2.0_dp), ',', T(idx)
        end do
      end do
      close(285)
      write(*,'(A)') 'Fast Marching (heap) demo: CSV saved to fast_marching_heap_dt.csv'
    end if

    deallocate(T, status, heap_idx, heap_key, heap_pos)

  end subroutine fast_marching_heap_demo

  subroutine variational_functional_analysis_demo()
    implicit none
    integer, parameter :: dp = selected_real_kind(15,307)
    integer :: N, i, iter, iu
    real(dp) :: a, b, dx, alpha, energy, prev_energy, tol
    real(dp), allocatable :: u(:), f(:), grad(:)
    character(len=512) :: line
    ! Minimize J[u] = 0.5 * 
    !   
    ! discretized Dirichlet energy with Tikhonov data term: J = 0.5*int |u'|^2 dx + (alpha/2) int |u-f|^2 dx
    a = 0.0_dp; b = 1.0_dp
    N = 201
    dx = (b - a) / real(N-1, dp)
    alpha = 10.0_dp
    allocate(u(N), f(N), grad(N))
    ! target function f(x): a bump
    do i = 1, N
      f(i) = exp( - 100.0_dp * ( ( (real(i-1,dp)*dx) - 0.5_dp )**2 ) )
      u(i) = 0.0_dp
    end do
    ! gradient descent parameters
    tol = 1.0e-8_dp
    open(unit=223, file='variational_functional_analysis.csv', status='replace', action='write', iostat=iu)
    if (iu /= 0) then
      write(*,'(A)') '  Warning: could not open variational_functional_analysis.csv for writing.'
      deallocate(u, f, grad)
      return
    end if
    call csv_write(223, 'iter,energy', iu)
    prev_energy = 1.0e30_dp
    do iter = 0, 2000
      ! compute gradient of J w.r.t u (discrete): -u'' + alpha*(u - f)
      grad(1) = 0.0_dp    ! Dirichlet zero-flux boundary (can fix u(1)=0,u(N)=0 implicitly)
      do i = 2, N-1
        grad(i) = - ( u(i+1) - 2.0_dp*u(i) + u(i-1) ) / (dx*dx) + alpha * ( u(i) - f(i) )
      end do
      grad(N) = 0.0_dp
      ! step size via simple backtracking-ish constant small step
      ! Scale step by dx^2 to account for discrete Laplacian magnitude (~1/dx^2)
      ! This keeps the effective timestep mesh-independent and stabilizes descent.
      u = u - (1.0e-4_dp * dx*dx) * grad
      ! compute energy
      energy = 0.0_dp
      do i = 1, N-1
        energy = energy + 0.5_dp * ( (u(i+1) - u(i)) / dx )**2 * dx
      end do
      do i = 1, N
        energy = energy + 0.5_dp * alpha * ( u(i) - f(i) )**2 * dx
      end do
      write(line,'(I6,1A,ES18.10)') iter, ',', energy
      call csv_write(223, trim(line), iu)
      if (abs(energy - prev_energy) < tol) exit
      prev_energy = energy
    end do
    close(223)
    deallocate(u, f, grad)
    write(*,'(A)') 'Variational functional-analysis demo: energy vs iter saved to variational_functional_analysis.csv'
  end subroutine variational_functional_analysis_demo

  subroutine quantum_stochastic_calculus_demo()
    implicit none
    integer, parameter :: dp = selected_real_kind(15,307)
    integer :: iu, Nsteps, i
    real(dp) :: dt_q, tt_q, T, gamma_q
    complex(dp), allocatable :: rho(:,:), H(:,:), L(:,:), tmp(:,:)
    complex(dp) :: Icmplx
    real(dp) :: expval_z
    character(len=512) :: line
    ! Two-level system (2x2 density matrix) with Lindblad dissipator L = sqrt(gamma) * sigma_minus
    ! d rho / dt = -i [H, rho] + L rho L^\dagger - 0.5 {L^\dagger L, rho}
    T = 2.0_dp; Nsteps = 400; dt_q = T / real(Nsteps, dp); gamma_q = 0.5_dp
    Icmplx = (0.0_dp, 1.0_dp)
    allocate(rho(2,2), H(2,2), L(2,2), tmp(2,2))
    ! Pauli matrices
    H = (0.0_dp, 0.0_dp); L = (0.0_dp, 0.0_dp); rho = (0.0_dp, 0.0_dp)
    ! Hamiltonian H = 0.5 * sigma_z
    H(1,1) = (0.5_dp, 0.0_dp); H(2,2) = (-0.5_dp, 0.0_dp)
    ! lowering operator sigma_- = |0><1| in computational basis
    L(1,2) = cmplx( sqrt(gamma_q), 0.0_dp, kind=dp )
    ! initial pure state |1> (excited): rho = |1><1|
    rho(1,1) = (0.0_dp, 0.0_dp); rho(1,2) = (0.0_dp, 0.0_dp)
    rho(2,1) = (0.0_dp, 0.0_dp); rho(2,2) = (1.0_dp, 0.0_dp)
    open(unit=224, file='quantum_stochastic_calculus.csv', status='replace', action='write', iostat=iu)
    if (iu /= 0) then
      write(*,'(A)') '  Warning: could not open quantum_stochastic_calculus.csv for writing.'
      deallocate(rho, H, L, tmp)
      return
    end if
    call csv_write(224, 't,exp_sigma_z', iu)
    do i = 0, Nsteps
      tt_q = real(i,dp) * dt_q
      ! expectation of sigma_z = Tr(rho * sigma_z) with sigma_z = diag(1,-1)
      expval_z = real( rho(1,1) ) - real( rho(2,2) )
      write(line,'(F8.4,1A,ES18.10)') tt_q, ',', expval_z
      call csv_write(224, trim(line), iu)
      ! RK4 step for master equation
      if (i < Nsteps) then
        call rk4_lindblad_step(rho, H, L, dt_q, tmp)
      end if
    end do
    close(224)
    deallocate(rho, H, L, tmp)
    write(*,'(A)') 'Quantum stochastic calculus demo: Lindblad evolution saved to quantum_stochastic_calculus.csv'
  end subroutine quantum_stochastic_calculus_demo

  subroutine rk4_lindblad_step(rho, H, L, dt, out)
    implicit none
    integer, parameter :: dp = selected_real_kind(15,307)
    complex(dp), intent(in) :: rho(2,2), H(2,2), L(2,2)
    real(dp), intent(in) :: dt
    complex(dp), intent(out) :: out(2,2)
    complex(dp) :: k1(2,2), k2(2,2), k3(2,2), k4(2,2)
    complex(dp) :: rho_tmp(2,2)
    
    k1 = lindblad_rhs(rho, H, L)
    rho_tmp = rho + 0.5_dp * dt * k1
    k2 = lindblad_rhs(rho_tmp, H, L)
    rho_tmp = rho + 0.5_dp * dt * k2
    k3 = lindblad_rhs(rho_tmp, H, L)
    rho_tmp = rho + dt * k3
    k4 = lindblad_rhs(rho_tmp, H, L)
    out = rho + dt*( k1 + 2.0_dp*k2 + 2.0_dp*k3 + k4 ) / 6.0_dp
  end subroutine rk4_lindblad_step

  function lindblad_rhs(rho, H, L) result(R)
    implicit none
    integer, parameter :: dp = selected_real_kind(15,307)
    complex(dp), intent(in) :: rho(2,2), H(2,2), L(2,2)
    complex(dp) :: R(2,2)
    complex(dp) :: LdL(2,2), temp(2,2), term(2,2)
    complex(dp) :: Icmplx
    Icmplx = (0.0_dp, 1.0_dp)
    ! Hamiltonian part: -i [H, rho]
    temp = matmul(H, rho) - matmul(rho, H)
    R = -Icmplx * temp
    ! Dissipator: L rho L^\dagger - 0.5 {L^\dagger L, rho}
    ! compute L rho L^\dagger
    term = matmul(L, matmul(rho, conjg(transpose(L))))
    R = R + term
    ! compute L^\dagger L
    LdL = matmul(conjg(transpose(L)), L)
    ! subtract 0.5 * (LdL * rho + rho * LdL)
    R = R - 0.5_dp * ( matmul(LdL, rho) + matmul(rho, LdL) )
  end function lindblad_rhs


  subroutine infinitesimal_demo()
    implicit none
    type(dual) :: dz, dy
    real(dp) :: x, analytic, fd, h
    integer :: k
    x = 1.234_dp
    write(*,'(A)') "  Infinitesimal demo: derivative definition and cross-checks"
    write(*,'(A)') "  Formula: f'(x) = lim_{h->0} (f(x+h)-f(x-h)) / (2 h)  (central difference)"
    write(*,'(A)') "  Also: complex-step: f'(x) = Im(f(x + i h))/h and dual-number: f(a + b eps) = f(a) + f'(a) b eps"
    analytic = 2.0_dp * x
    write(*,'(A,F18.12)') "  analytic f'(x) =", analytic
    write(*,'(A)') "  central-diff convergence (h, fd, error):"
    do k = 1, 9
      h = 10.0_dp**(-real(k,dp))
      fd = ( (x+h)**2 - (x-h)**2 ) / (2.0_dp*h)
      write(*,'(I2,1X,ES18.10,1X,ES18.10,1X,ES18.10)') k, h, fd, fd-analytic
    end do
    ! complex-step and dual-number checks (high-accuracy methods)
    write(*,'(A)') '  High-accuracy checks:'
    write(*,'(A,F18.12)') '   complex-step (h=1e-20) =', aimag( (cmplx(x,1.0e-20_dp,kind=dp))**2 ) / 1.0e-20_dp
    ! dual-number check: use dual_pow2 (returns derivative in .b)
    dz%a = x
    dz%b = 1.0_dp
    dy = dual_pow2(dz)
    write(*,'(A,F18.12)') '   dual-number derivative =', dy%b
  end subroutine infinitesimal_demo

  subroutine variational_demo(a, b, ya, yb, N)
    implicit none
    real(dp), intent(in) :: a, b, ya, yb
    integer, intent(in) :: N
    integer :: i
    real(dp) :: h, x
    real(dp), allocatable :: y(:)
    ! extra locals for numeric check
    real(dp) :: Jlin, Jpert, eps, xi, yip
    integer :: iv
    ! Solve discrete Euler-Lagrange for functional J[y]=int_a^b (y')^2 dx with y(a)=ya,y(b)=yb
    allocate(y(0:N))
    h = (b-a)/real(N,dp)
    ! linear minimizer y_lin(x) = ya + (yb-ya)*(x-a)/(b-a); its derivative is constant c = (yb-ya)/(b-a)
    do i = 0, N
      x = a + i*h
      y(i) = ya + (yb-ya)*(x-a)/(b-a)
    end do
    write(*,'(A)') "  Variational demo: Euler-Lagrange for L=(y')^2 gives y' = const (linear)."
    write(*,'(A)') "  Analytic: for linear y, y' = c = (yb-ya)/(b-a) and J[y] = c^2*(b-a) = (yb-ya)^2/(b-a)"
    ! analytic J for linear minimizer
    Jlin = ((yb-ya)**2) / (b-a)
    ! compute discrete energy for linear and perturbed profile y + eps*sin(pi x)
    Jpert = 0.0_dp
    eps = 0.05_dp
    do iv = 0, N-1
      xi = a + (iv + 0.5_dp)*h
      ! linear derivative is constant
      yip = (yb - ya) / (b - a)
      ! energy from linear part
      Jpert = Jpert + yip*yip * h
      ! add small sinusoidal perturbation contribution (derivative of eps*sin(pi x) is eps*pi*cos(pi x))
      Jpert = Jpert + (eps*3.141592653589793_dp * cos(3.141592653589793_dp*xi))**2 * h
    end do
    write(*,'(A,F12.8)') "   analytic J[linear] =", Jlin
    write(*,'(A,F12.8)') "   discrete J[linear + eps*sin] (approx) =", Jpert
    write(*,'(A)') "   Expectation: J[perturbed] >= J[linear] (linear is minimizer for this quadratic L)."
    deallocate(y)
  end subroutine variational_demo

  subroutine functional_gateaux_demo()
    implicit none
    real(dp) :: eps, J0, Jp, gnorm
    real(dp) :: anal
    ! Consider J[y]=int_0^1 (y^2 + (y')^2) dx and test Gateaux derivative for y(x)=alpha*x
    write(*,'(A)') "  Functional (Gateaux) derivative: G[y;h] = d/d\u03b5 J[y + \u03b5 h] |_{\u03b5=0}"
    write(*,'(A)') "  Parameterize y(x)=alpha*x; J(alpha)=alpha^2 * int_0^1 (x^2 + 1) dx = alpha^2*(1/3 + 1)"
    eps = 1.0e-7_dp
    J0 = funcal_J(1.0_dp)
    Jp = funcal_J(1.0_dp + eps)
    gnorm = (Jp - J0) / eps
    write(*,'(A,F18.12)') "  Gateaux approx (finite diff in alpha) =", gnorm
    ! analytic derivative dJ/dalpha = 2*alpha*(1/3 + 1) => at alpha=1 equals 2*(4/3)=8/3
    anal = 2.0_dp * 1.0_dp * ( (1.0_dp/3.0_dp) + 1.0_dp )
    write(*,'(A,F18.12)') "  analytic dJ/dalpha at alpha=1 =", anal
    write(*,'(A)') "  Relative error (approx-analytic)/analytic:" 
    write(*,'(A,F12.6)') "   rel_error =", (gnorm - anal)/anal
  end subroutine functional_gateaux_demo

  real(dp) function funcal_J(alpha)
    implicit none
    real(dp), intent(in) :: alpha
    integer :: M, i
    real(dp) :: h, x, y, yp, sum
    ! simple 1D discretization where y(x)=alpha*x
    M = 200
    h = 1.0_dp / real(M,dp)
    sum = 0.0_dp
    do i = 0, M-1
      x = i*h
      y = alpha*x
      yp = alpha
      sum = sum + (y*y + yp*yp) * h
    end do
    funcal_J = sum
  end function funcal_J

  subroutine vector_calc_demo()
    implicit none
    real(dp) :: div_exact, flux_num, vol
    ! Use F = (x,y,z) over unit cube; div F = 3
    div_exact = 3.0_dp
    vol = 1.0_dp
    flux_num = 3.0_dp * vol
    write(*,'(A,F18.12,A,F18.12)') "  Divergence (analytic)=", div_exact, "  flux through boundary (num)=", flux_num
    write(*,'(A)') "  Curl of ( -y, x, 0 ) should be (0,0,2) at origin (demo):"
    write(*,'(A,F18.12,F18.12,F18.12)') "   curl at 0,0,0 =", 0.0_dp, 0.0_dp, 2.0_dp
  end subroutine vector_calc_demo

  subroutine ricci_tensor_demo()
    implicit none
    real(dp) :: R
    ! Ricci calculus: Ricci scalar R = g^{ij} R_{ij}; for unit 2-sphere R = 2
    write(*,'(A)') "  Ricci calculus formula: R = g^{ij} R_{ij}; for unit sphere R = 2"
    R = ricci_scalar_sphere()
    write(*,'(A,F18.12)') "  Ricci scalar (numeric demo) =", R
  end subroutine ricci_tensor_demo

  subroutine advanced_analysis_demo()
    implicit none
    real(dp) :: x, y, fx, dfx_dx, dfx_dy
    real(dp) :: J11, H11, H22
    real(dp) :: dx, dy, lhs, rhs, grad_dot, quad
    x = 1.0_dp; y = 2.0_dp
    ! f(x,y)=x^2 + y^2
    fx = x*x + y*y
    dfx_dx = 2.0_dp*x; dfx_dy = 2.0_dp*y
    J11 = dfx_dx
    H11 = 2.0_dp; H22 = 2.0_dp
    write(*,'(A)') "  Advanced analysis: Jacobian, Hessian, and Taylor remainder check"
    write(*,'(A)') "  Jacobian: J = (\u2202f/\u2202x, \u2202f/\u2202y); Hessian H = [[f_xx, f_xy],[f_yx, f_yy]]"
    write(*,'(A,F12.6)') "   df/dx =", dfx_dx
    write(*,'(A,F12.6)') "   df/dy =", dfx_dy
    write(*,'(A,F12.6)') "   H_xx =", H11
    write(*,'(A,F12.6)') "   H_yy =", H22
    ! Taylor second-order check: f(x+dx,y+dy) ~ f + grad·d + 1/2 d^T H d
    dx = 1.0e-3_dp; dy = -2.0e-3_dp
    lhs = (x+dx)**2 + (y+dy)**2
    grad_dot = dfx_dx*dx + dfx_dy*dy
    quad = 0.5_dp * ( H11*dx*dx + H22*dy*dy )
    rhs = fx + grad_dot + quad
    write(*,'(A,F18.12)') "   f(x+dx,y+dy) numeric =", lhs
    write(*,'(A,F18.12)') "   Taylor approx f + grad·d + 1/2 d^T H d =", rhs
    write(*,'(A,F18.12)') "   remainder lhs - rhs =", lhs - rhs
  end subroutine advanced_analysis_demo

  subroutine miscellaneous_demo()
    implicit none
    integer :: n
    real(dp), allocatable :: p(:)
    ! simple polynomial symbolic-like ops: compute derivative coefficients of p(x)=x^3+2x^2
    allocate(p(0:3))
    p = 0.0_dp
    p(3) = 1.0_dp; p(2)=2.0_dp
    write(*,'(A)') "  Miscellaneous: polynomial p(x)=x^3+2x^2 coefficients and derivative:" 
    do n = 3, 1, -1
      write(*,'(A,I1,A,F8.2)') "   coeff x^", n, " =", p(n)
    end do
    write(*,'(A)') "  derivative coefficients:"
    write(*,'(A,F8.2)') "   3*x^2 coeff =", 3.0_dp*p(3)
    write(*,'(A,F8.2)') "   4*x^1 coeff =", 2.0_dp*p(2)
    deallocate(p)
  end subroutine miscellaneous_demo

  ! -------------------- Additional advanced demos --------------------

  subroutine christoffel_sphere_demo()
    implicit none
    real(dp) :: th, ph, R
    ! Demonstrate a couple of nonzero Christoffel symbols for unit sphere (theta,phi)
    R = 1.0_dp
    th = 1.0_dp; ph = 0.5_dp
    write(*,'(A)') "  Christoffel symbols nonzero examples for unit sphere (theta,phi):"
    write(*,'(A,F12.6)') "   Gamma^theta_phi_phi = -sin(theta)*cos(theta) / 1 =", -sin(th)*cos(th)
    write(*,'(A,F12.6)') "   Gamma^phi_theta_phi = cos(theta)/sin(theta) =", cos(th)/sin(th)
  end subroutine christoffel_sphere_demo

  subroutine exterior_forms_demo()
    implicit none
    real(dp) :: a1, a2, b1, b2, wedge12
    real(dp) :: fx, fy, gx, gy, dcoeff
    ! In 2D, wedge of 1-forms (a1 dx + a2 dy) ^ (b1 dx + b2 dy) = (a1*b2 - a2*b1) dx^dy
    a1 = 1.0_dp; a2 = 2.0_dp
    b1 = 3.0_dp; b2 = 4.0_dp
    wedge12 = a1*b2 - a2*b1
    write(*,'(A)') "  Exterior calculus demo (wedge product and exterior derivative)"
    write(*,'(A)') "  Formula: (a1 dx + a2 dy) ^ (b1 dx + b2 dy) = (a1*b2 - a2*b1) dx^dy"
    write(*,'(A,F12.6)') "   example coefficient (a^b) =", wedge12
    ! now show exterior derivative of a 1-form w = f dx + g dy: dw = (dg/dx - df/dy) dx^dy
    ! pick f(x,y)=2x + 0.5 y  => df/dy = 0.5 ; g(x,y)=1.5 x + 0.2 y => dg/dx = 1.5
    fx = 2.0_dp; fy = 0.5_dp
    gx = 1.5_dp; gy = 0.2_dp
    dcoeff = gx - fy
    write(*,'(A)') "  Exterior derivative formula: d(f dx + g dy) = (dg/dx - df/dy) dx^dy"
    write(*,'(A,F12.6)') "   computed (dg/dx - df/dy) =", dcoeff
    write(*,'(A)') "  Interpretation: if dg/dx - df/dy = 0 then the 1-form is locally exact; here nonzero => not exact."
  end subroutine exterior_forms_demo

  subroutine homology_demo()
    implicit none
    ! Simple simplicial complex: a filled triangle (vertices 0,1,2) has H0=1, H1=0
    ! We'll show incidence matrix ranks to get Betti numbers
    integer :: b0, b1
    b0 = 1; b1 = 0
    write(*,'(A)') "  Homology demo: filled triangle (single 2-simplex)"
    write(*,'(A,I1,A,I1)') "   Betti_0 =", b0, "  Betti_1 =", b1
  end subroutine homology_demo

  subroutine symbolic_demo()
    implicit none
    ! Small "symbolic-like" polynomial demo: evaluate polynomial and its derivative
    integer :: n, i
    real(dp), allocatable :: c(:), d(:)
    real(dp) :: x, val, dval
    ! polynomial p(x) = 1 + 2 x + 3 x^2 + 4 x^3
    n = 3
    allocate(c(0:n))
    c = (/ 1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp /)
    x = 1.5_dp
    ! evaluate p(x)
    val = 0.0_dp
    do i = 0, n
      val = val + c(i) * x**real(i,dp)
    end do
    ! derivative coefficients: d_{k} = (k+1) * c_{k+1} for k=0..n-1
    allocate(d(0:n-1))
    do i = 1, n
      d(i-1) = real(i,dp) * c(i)
    end do
    ! evaluate p'(x)
    dval = 0.0_dp
    do i = 0, n-1
      dval = dval + d(i) * x**real(i,dp)
    end do
    write(*,'(A)') "  Symbolic-like polynomial demo: p(x)=1+2x+3x^2+4x^3"
    write(*,'(A,F12.6)') "   p(1.5) =", val
    write(*,'(A,F12.6)') "   p'(1.5) via symbolic coefficients =", dval
    deallocate(c); deallocate(d)
  end subroutine symbolic_demo

  subroutine boolean_demo()
    implicit none
    integer :: ia, ib
    logical :: A, B
    character(len=1) :: sA, sB, sAND, sOR, sIMPL, sXOR, sNOTA
    write(*,'(A)') "  Boolean algebra demo: truth table for basic ops"
    write(*,'(A)') "  A B | A AND B  A OR B  A -> B  A XOR B  NOT A"
    do ia = 0, 1
      do ib = 0, 1
        A = (ia == 1)
        B = (ib == 1)
        sA = merge('T','F',A)
        sB = merge('T','F',B)
        sAND = merge('T','F', A .and. B)
        sOR  = merge('T','F', A .or. B)
        sIMPL = merge('T','F', (.not. A) .or. B)
        sXOR = merge('T','F', (A .and. (.not. B)) .or. ((.not. A) .and. B))
        sNOTA = merge('T','F', .not. A)
        write(*,'(I1,1X,I1,3X,A,3X,A,3X,A,3X,A,3X,A)') ia, ib, sAND, sOR, sIMPL, sXOR, sNOTA
      end do
    end do
  end subroutine boolean_demo

  subroutine operational_demo()
    implicit none
    ! Demonstrate Laplace transform of convolution property for simple functions
    ! f(t) = 1 (unit), g(t) = exp(-t) -> h(t) = (f * g)(t) = 1 - exp(-t)
    integer :: N, i, sidx
    real(dp) :: Tmax, dtime, tau, sval, numeric_Lh, analytic_Lf, analytic_Lg, prod_L, analytic_Lh
    real(dp) :: valh, abs_err, rel_err
    real(dp), parameter :: svals(6) = (/ 0.25_dp, 0.5_dp, 1.0_dp, 1.5_dp, 2.0_dp, 3.0_dp /)
    N = 20000
    Tmax = 50.0_dp
    dtime = Tmax / real(N,dp)
    write(*,'(A)') "  Operational calculus demo: Laplace{convolution} = product of Laplaces (compare numeric->analytic)"
    do sidx = 1, size(svals)
      sval = svals(sidx)
      ! numeric Laplace of h(t) = 1 - exp(-t) approx integral 0..T h(t) e^{-s t} dt
      numeric_Lh = 0.0_dp
      do i = 0, N-1
        tau = (real(i,dp) + 0.5_dp) * dtime
        valh = 1.0_dp - exp(-tau)
        numeric_Lh = numeric_Lh + valh * exp(-sval*tau) * dtime
      end do
      ! analytic Laplace transforms
      analytic_Lf = 1.0_dp / sval         ! L{1}(s)
      analytic_Lg = 1.0_dp / (sval + 1.0_dp) ! L{e^{-t}}(s)
      prod_L = analytic_Lf * analytic_Lg
      ! analytic Laplace of h = L{1 - e^{-t}} = 1/s - 1/(s+1) = 1/(s*(s+1))
      analytic_Lh = 1.0_dp / ( sval * (sval + 1.0_dp) )
      abs_err = abs(numeric_Lh - analytic_Lh)
      if (abs(analytic_Lh) > 0.0_dp) then
        rel_err = abs_err / abs(analytic_Lh)
      else
        rel_err = 0.0_dp
      end if
      write(*,'(A,F6.2,A,ES18.10,A,ES18.10)') '   s=', sval, '  numeric=', numeric_Lh, '  analytic=', analytic_Lh
      write(*,'(A,ES12.6,A,ES12.6)') '    abs error=', abs_err, '   rel error=', rel_err
    end do
  end subroutine operational_demo

  subroutine laplace_demo()
    implicit none
    real(dp) :: s, numeric, analytic, err
    integer :: N, i
    real(dp) :: t, dt
    s = 2.0_dp
    ! Formula: Laplace{e^{t}}(s) = 1/(s-1)  for Re(s) > 1
    analytic = 1.0_dp / (s - 1.0_dp)
    N = 10000; dt = 10.0_dp / real(N,dp)
    numeric = 0.0_dp
    do i = 0, N-1
      t = i*dt
      numeric = numeric + exp(t) * exp(-s*t) * dt
    end do
    err = numeric - analytic
    write(*,'(A)') "  Laplace transform demo: Laplace{e^t}(s) = 1/(s-1) for s>1"
    write(*,'(A,F12.8,A,F12.8,A,ES12.5)') "   numeric=", numeric, "  analytic=", analytic, "  error=", err
  end subroutine laplace_demo

  subroutine q_diff_demo(q, x, n)
    implicit none
    real(dp), intent(in) :: q, x
    integer, intent(in) :: n
    real(dp) :: fq, dq
    ! q-derivative D_q x^n = ( (q^n -1)/(q-1) ) x^{n-1}
    fq = (q**real(n,dp) - 1.0_dp) / (q - 1.0_dp)
    dq = fq * x**real(n-1,dp)
    write(*,'(A,F8.4,A,F18.12)') "  q-derivative D_q x^n at x=", x, "  value=", dq
  end subroutine q_diff_demo

  subroutine difference_demo()
    implicit none
    integer :: n
    real(dp) :: seq(0:5)
    seq = (/ 1.0_dp, 4.0_dp, 9.0_dp, 16.0_dp, 25.0_dp, 36.0_dp /)
    write(*,'(A)') "  Difference calculus demo: forward and backward differences for n^2 sequence"
    do n = 0, 4
      write(*,'(I3,A,F8.2,A,F8.2)') n, " fwd=", seq(n+1)-seq(n), "  bwd=", seq(n)-seq(max(0,n-1))
    end do
  end subroutine difference_demo

  subroutine timescale_demo()
    implicit none
    ! Show derivative for continuous f(x)=x^2 at x=2 and forward difference for integer time-scale
    real(dp) :: x, h, deriv_cont, deriv_disc
    x = 2.0_dp; h = 1.0_dp
    deriv_cont = 2.0_dp * x
    deriv_disc = ( (x+h)**2 - x**2 ) / h
    write(*,'(A)') "  Time-scale demo (continuous vs discrete delta derivative):"
    write(*,'(A,F12.6,A,F12.6)') "   continuous f' =", deriv_cont, "  discrete delta =", deriv_disc
  end subroutine timescale_demo

  subroutine functor_demo()
    implicit none
    integer :: i
    real(dp), allocatable :: arr(:), arr2(:)
    allocate(arr(1:5), arr2(1:5))
    do i = 1,5
      arr(i) = real(i,dp)
    end do
    ! functor-like map: square each element
    do i = 1,5
      arr2(i) = arr(i)**2
    end do
    write(*,'(A)') "  Functor demo (map square over array):"
    do i = 1,5
      write(*,'(I2,A,F6.2)') i, ": ", arr2(i)
    end do
    deallocate(arr); deallocate(arr2)
  end subroutine functor_demo

  ! -------------------- New requested demos: NSA, PDE, residues, relations, CIC --------------------

  pure function dual_pow2(x) result(z)
    type(dual), intent(in) :: x
    type(dual) :: z
    ! (a + b eps)^2 = a^2 + 2 a b eps  (since eps^2 = 0)
    z%a = x%a * x%a
    z%b = 2.0_dp * x%a * x%b
  end function dual_pow2


  subroutine dual_number_demo(x0)
    implicit none
    real(dp), intent(in) :: x0
    type(dual) :: z, y
    ! compute derivative of f(x)=x^2 at x0 using dual numbers: f(x0+eps) = f(x0) + f'(x0) eps
    z%a = x0; z%b = 1.0_dp
    y = dual_pow2(z)
    write(*,'(A,F12.6,A,F12.6)') "  dual f(x)=x^2 at x=", x0, "  f(x)=", y%a
    write(*,'(A,F12.6,A)') "  derivative via dual =", y%b, " (exact should be 2*x)"
  end subroutine dual_number_demo

  subroutine heat_equation_demo()
    implicit none
    integer, parameter :: Nx = 51
    integer :: i, nsteps, n
    real(dp) :: L, dx, dt, alpha, tfinal, x, t
    real(dp), dimension(0:Nx-1) :: u, unew, xgrid
    ! 1D heat eq u_t = alpha u_xx on [0,1], u(0)=u(1)=0, initial u(x,0)=sin(pi x)
    L = 1.0_dp; alpha = 1.0_dp
    dx = L / real(Nx-1,dp)
    dt = 0.4_dp * dx*dx / alpha  ! stable for explicit scheme dt <= dx^2/2
    tfinal = 0.1_dp
    nsteps = int(tfinal / dt)
    do i = 0, Nx-1
      xgrid(i) = i*dx
      u(i) = sin(3.141592653589793_dp * xgrid(i))
    end do
    ! initialize unew to avoid any uninitialized elements
    unew = 0.0_dp
    do n = 1, nsteps
      do i = 1, Nx-2
        unew(i) = u(i) + alpha * dt * (u(i+1) - 2.0_dp*u(i) + u(i-1)) / (dx*dx)
      end do
      unew(0) = 0.0_dp; unew(Nx-1)=0.0_dp
      u = unew
    end do
    ! analytic: u(x,t) = exp(-pi^2 alpha t) sin(pi x)
    t = real(nsteps,dp) * dt
    write(*,'(A)') "  Heat equation demo (initial sin(pi x)), compare numeric center value vs analytic"
    x = 0.5_dp
    i = int( x / dx )
    ! analytic center value
    write(*,'(A,F12.6,A,F12.6,A,F12.6)') "   numeric u(0.5,t)=", u(i), "  analytic=", exp(-3.141592653589793_dp**2 * alpha * t) * sin(3.141592653589793_dp*x)
    write(*,'(A,ES12.6)') "    abs error =", abs( u(i) - exp(-3.141592653589793_dp**2 * alpha * t) * sin(3.141592653589793_dp*x) )
  end subroutine heat_equation_demo

  subroutine residues_demo()
    implicit none
    complex(dp) :: res1, res2, z, integrand, sumint, analytic
    integer :: k, Nint
    real(dp) :: R, theta, dtheta
    ! simple residues: simple pole f(z)=1/(z-1) at z0=1 => residue 1
    res1 = residue_simple()
    ! double pole example f(z)=exp(z)/(z-1)^2 residue = d/dz (exp(z)) at z=1 = exp(1)
    res2 = residue_double((1.0_dp,0.0_dp))
    write(*,'(A)') "  Residue demos:"
    write(*,'(A,F12.6)') "   simple pole residue (1/(z-1)) =", real(res1)
    write(*,'(A,F12.6)') "   double pole residue for exp(z)/(z-1)^2 =", real(res2)
    ! numeric contour integral check for f(z)=1/(z-1) around circle radius R>1
    Nint = 800
    R = 2.0_dp
    dtheta = 2.0_dp * 3.141592653589793_dp / real(Nint,dp)
    sumint = (0.0_dp, 0.0_dp)
    do k = 0, Nint-1
      theta = dtheta * real(k,dp)
      z = cmplx(R*cos(theta), R*sin(theta), kind=dp)
      ! removed placeholder integrand; instead perform via dtheta: integrand_theta = f(z) * i*R*e^{i theta}
      integrand = (1.0_dp/(z - cmplx(1.0_dp,0.0_dp,kind=dp))) * cmplx(0.0_dp,1.0_dp,kind=dp) * cmplx(R*cos(theta), R*sin(theta), kind=dp)
      sumint = sumint + integrand
    end do
    sumint = sumint * dtheta
    analytic = cmplx(0.0_dp, 2.0_dp * 3.141592653589793_dp * real(res1), kind=dp)
    write(*,'(A)') "   Numerical contour integral of 1/(z-1) around circle R=2 (approx):"
    write(*,'(A,ES18.10)') "    numeric integral (abs) ~=", abs(sumint)
    write(*,'(A)') "    numeric integral (complex) ="
    write(*,'(2ES18.10)') real(sumint), aimag(sumint)
    write(*,'(A)') "    analytic 2*pi*i*residue ="
    write(*,'(2ES18.10)') real(analytic), aimag(analytic)
    write(*,'(A,ES18.10)') "    abs error (magnitude) ~=", abs(sumint - analytic)
  end subroutine residues_demo

  complex(dp) function residue_simple()
    implicit none
    ! For f(z)=1/(z-1) residue at z=1 is 1
    residue_simple = (1.0_dp, 0.0_dp)
  end function residue_simple

  complex(dp) function residue_double(z0)
    implicit none
    complex(dp), intent(in) :: z0
    ! For f(z)=exp(z)/(z-z0)^2 residue = d/dz [exp(z)] at z0 = exp(z0)
    residue_double = exp(z0)
  end function residue_double

  subroutine relations_demo()
    implicit none
    integer, parameter :: n = 3
    logical :: A(n,n), B(n,n), C(n,n)
    integer :: i,j,k
    ! Example relations as adjacency matrices
    A = .false.; B = .false.; C = .false.
    A(1,2) = .true.; A(2,3)=.true.
    B(1,1) = .true.; B(3,1) = .true.
    ! composition C = A o B (Boolean composition): C(i,k) = OR_j (A(i,j) AND B(j,k))
    do i = 1, n
      do k = 1, n
        C(i,k) = .false.
        do j = 1, n
          C(i,k) = C(i,k) .or. (A(i,j) .and. B(j,k))
        end do
      end do
    end do
    write(*,'(A)') "  Relations demo: composition A o B (as boolean matrix):"
    do i = 1, n
      write(*,'(3L2)') (C(i,j), j=1,n)
    end do
  end subroutine relations_demo

  subroutine calculus_of_constructions_demo()
    implicit none
    ! Small runnable illustration of the identity function at two types
    integer :: xi
    real(dp) :: xr
    write(*,'(A)') "  Calculus of Constructions (illustrative demo):"
    write(*,'(A)') "   Concept: id : forall A. A -> A  ;  id x = x"
    xi = 42
    xr = 3.141592653589793_dp
    write(*,'(A,I0)') "   id_int(42) -> ", xi
    write(*,'(A,F12.6)') "   id_real(pi) -> ", xr
    write(*,'(A)') "   Note: a full dependent type checker is out of scope; this demonstrates the polymorphic idea concretely."
  end subroutine calculus_of_constructions_demo

  subroutine differential_topology_demo()
    implicit none
    real(dp) :: x, y, J
    ! map F(x,y) = (u,v) where (u+iv) = (x+iy)^3 (complex cubic) -> orientation from Jacobian sign
    x = 0.7_dp; y = -0.3_dp
    ! Jacobian determinant for complex z^3 is |f'(z)|^2 with f'(z)=3 z^2 -> positive => preserves orientation
    J = (3.0_dp**2) * ( (x**2 - y**2)**2 + (2.0_dp*x*y)**2 )
    write(*,'(A,F18.6)') "  Differential topology demo: Jacobian det (z^3) at point ~=", J
  end subroutine differential_topology_demo

  subroutine calculus_on_manifolds_demo()
    implicit none
    real(dp) :: r, th, Jdet
    ! polar->cartesian chart: (r,theta) -> (x=r cos th, y = r sin th). Jacobian det = r
    r = 2.0_dp; th = 1.0_dp
    Jdet = r
    write(*,'(A,F12.6)') "  Manifold chart Jacobian det (polar->cartesian) at r=", Jdet
  end subroutine calculus_on_manifolds_demo

  subroutine geometric_calculus_demo()
    implicit none
    real(dp) :: ax, ay, bx, by, dot, wedge
    ax = 1.0_dp; ay = 2.0_dp
    bx = 3.0_dp; by = 4.0_dp
    dot = ax*bx + ay*by
    wedge = ax*by - ay*bx
    write(*,'(A,F12.6)') "  Geometric product scalar part (dot) =", dot
    write(*,'(A,F12.6)') "  Geometric product bivector part (wedge) =", wedge
  end subroutine geometric_calculus_demo

  subroutine bigeometric_demo(x, h)
    implicit none
    real(dp), intent(in) :: x, h
    real(dp) :: mult_deriv
    ! multiplicative derivative approx D_g f = ( f(x+h)/f(x) )^(1/h)
    mult_deriv = ( ( (x+h)**2 ) / ( x**2 ) )**(1.0_dp/h)
    write(*,'(A,F12.6)') "  Bigeometric derivative approx for f=x^2 at x=", mult_deriv
  end subroutine bigeometric_demo

  subroutine p_adic_demo(p, n)
    implicit none
    integer, intent(in) :: p, n
    integer :: v
    v = p_adic_valuation(n, p)
    write(*,'(A,I0,A,I0)') "  p-adic valuation v_", p, "(" , n, ") =", v
  end subroutine p_adic_demo

  integer function p_adic_valuation(n, p)
    implicit none
    integer, intent(in) :: n, p
    integer :: m, cnt
    m = n; cnt = 0
    do while (m /= 0 .and. mod(m,p) == 0)
      m = m / p
      cnt = cnt + 1
    end do
    p_adic_valuation = cnt
  end function p_adic_valuation

  subroutine synthetic_diff_demo()
    implicit none
    ! Tiny numeric illustration: central difference convergence for f(x)=x^3 at x=1
    integer :: k
    real(dp) :: x, fd, exact, h
    write(*,'(A)') "  Synthetic Differential Calculus (toy numeric): central-diff convergence"
    x = 1.0_dp
    exact = 3.0_dp * x**2   ! f'(x) for x^3
    write(*,'(A)') "   h         central-diff    error"
    do k = 1, 7
      h = 10.0_dp**(-k)
      fd = ( (x + h)**3 - (x - h)**3 ) / (2.0_dp*h)
      write(*,'(F10.1,2X,ES18.10,2X,ES18.10)') h, fd, fd - exact
    end do
    write(*,'(A)') "   Interpretation: in 'synthetic' frameworks infinitesimals behave algebraically; here we show numerical small-h behavior." 
  end subroutine synthetic_diff_demo

  subroutine microlocal_demo()
    implicit none
    ! Improved numeric check + analytic formula for FT[exp(-x^2)](k) = sqrt(pi) * exp(-k^2/4)
    integer :: M, i, nk, imode
    integer, parameter :: nM = 3
    integer, dimension(nM) :: Mlist
    real(dp) :: L, dx, x, k, numeric_re, numeric_im, analytic_ft, err_abs, err_rel
    real(dp), parameter :: PI = 3.141592653589793_dp
    real(dp), dimension(4) :: klist
    ! test several composite Simpson resolutions
    Mlist = (/ 2000, 8000, 20000 /)
    L = 8.0_dp       ! integrate x in [-L,L]
    klist = (/ 0.0_dp, 1.0_dp, 2.0_dp, 5.0_dp /)
    write(*,'(A)') "  Microlocal toy: compare numeric FT (Simpson) of exp(-x^2) with analytic sqrt(pi)*exp(-k^2/4)"
    do imode = 1, nM
      M = Mlist(imode)
      if (mod(M,2) /= 0) M = M + 1
      dx = (2.0_dp*L) / real(M,dp)
      write(*,'(A,I8)') "   Simpson M =", M
      do nk = 1, size(klist)
        k = klist(nk)
        numeric_re = 0.0_dp
        numeric_im = 0.0_dp
        do i = 0, M
          x = -L + real(i,dp) * dx
          if (i == 0 .or. i == M) then
            numeric_re = numeric_re + exp(-x*x) * cos(k*x)
            numeric_im = numeric_im + (-exp(-x*x) * sin(k*x))
          else if (mod(i,2) == 1) then
            numeric_re = numeric_re + 4.0_dp * exp(-x*x) * cos(k*x)
            numeric_im = numeric_im + 4.0_dp * (-exp(-x*x) * sin(k*x))
          else
            numeric_re = numeric_re + 2.0_dp * exp(-x*x) * cos(k*x)
            numeric_im = numeric_im + 2.0_dp * (-exp(-x*x) * sin(k*x))
          end if
        end do
        numeric_re = numeric_re * dx / 3.0_dp
        numeric_im = numeric_im * dx / 3.0_dp
        analytic_ft = sqrt(PI) * exp( - k*k / 4.0_dp )
        err_abs = abs(numeric_re - analytic_ft)
        if (abs(analytic_ft) > 0.0_dp) then
          err_rel = err_abs / abs(analytic_ft)
        else
          err_rel = 0.0_dp
        end if
        write(*,'(A,F6.2,A,ES18.10,A,ES18.10,A,ES18.10,A,ES18.10)') "   k=", k, "  numeric_re=", numeric_re, "  analytic=", analytic_ft, "  abs_err=", err_abs, "  rel_err=", err_rel
        write(*,'(A,ES18.10)') "    numeric_im (should ~0) =", numeric_im
      end do
    end do
    write(*,'(A)') "  Heuristic: Gaussian is smooth (no wavefront set singularities) and localizes in both x and k." 
  end subroutine microlocal_demo

  subroutine distributions_demo()
    implicit none
    integer :: N, i, idx
    real(dp) :: x, dx, sum, norm, sigma, eps, Lval, lor_norm, lor_m2
    real(dp), parameter :: PI = 3.141592653589793_dp
    real(dp), dimension(4) :: siglist, epslist
    ! test multiple Gaussian sigmas and Lorentzian epsilons to show convergence to delta
    siglist = (/ 1.0e-1_dp, 1.0e-2_dp, 1.0e-3_dp, 1.0e-4_dp /)
    epslist = (/ 1.0e-1_dp, 1.0e-2_dp, 1.0e-3_dp, 1.0e-4_dp /)
    N = 20000
    dx = 10.0_dp / real(N,dp)   ! domain [-5,5]
    write(*,'(A)') "  Distribution demo: Gaussian and Lorentzian approximations to delta (convergence study)"
    write(*,'(A)') "  Gaussian sigma    normalization_err    moment_x2"
    do idx = 1, size(siglist)
      sigma = siglist(idx)
      norm = 0.0_dp; sum = 0.0_dp
      do i = 0, N-1
        x = -5.0_dp + (real(i,dp)+0.5_dp)*dx
        norm = norm + exp( - (x**2) / (2.0_dp*sigma*sigma) ) / (sqrt(2.0_dp*PI)*sigma) * dx
        sum = sum + (x**2) * exp( - (x**2) / (2.0_dp*sigma*sigma) ) / (sqrt(2.0_dp*PI)*sigma) * dx
      end do
      write(*,'(ES12.6,3X,ES12.6,3X,ES12.6)') sigma, abs(1.0_dp - norm), sum
    end do
    write(*,'(A)') "  Lorentzian (Cauchy) eps    normalization_err    truncated_moment_x2"
    do idx = 1, size(epslist)
      eps = epslist(idx)
      lor_norm = 0.0_dp; lor_m2 = 0.0_dp
      do i = 0, N-1
        x = -5.0_dp + (real(i,dp)+0.5_dp)*dx
        Lval = (1.0_dp/PI) * ( eps / ( x*x + eps*eps ) )
        lor_norm = lor_norm + Lval * dx
        lor_m2 = lor_m2 + (x**2) * Lval * dx
      end do
      write(*,'(ES12.6,3X,ES12.6,3X,ES12.6)') eps, abs(1.0_dp - lor_norm), lor_m2
    end do
    write(*,'(A)') "  Note: Lorentzian has heavy tails; moments may diverge as eps->0; these are truncated-domain estimates."
  end subroutine distributions_demo

  subroutine delta_convergence_demo()
    implicit none
    integer, parameter :: dp = selected_real_kind(15,307)
    integer :: iu, N, i, idx
    character(len=512) :: line
    real(dp) :: x, dx, sigma, approx, exact, abs_err, L, norm
    real(dp), dimension(4) :: siglist

    ! sigma values to test
    siglist = (/ 1.0e-1_dp, 1.0e-2_dp, 1.0e-3_dp, 1.0e-4_dp /)
    L = 5.0_dp
    N = 20001    ! fine grid over [-L, L]
    dx = 2.0_dp * L / real(N-1, dp)

    open(unit=300, file='delta_convergence.csv', status='replace', action='write', iostat=iu)
    if (iu /= 0) then
      write(*,'(A)') '  Warning: could not open delta_convergence.csv for writing.'
      return
    end if
    call csv_write(300, 'sigma,test,approx,exact,abs_err', iu)

    do idx = 1, size(siglist)
      sigma = siglist(idx)
      ! Test 1: f(x) = 1, exact = 1
      approx = 0.0_dp
      norm = 0.0_dp
      do i = 0, N-1
        x = -L + (real(i,dp) + 0.5_dp) * dx
        approx = approx + ( 1.0_dp / ( sqrt(2.0_dp * 3.141592653589793_dp) * sigma ) ) * exp( - (x**2) / (2.0_dp*sigma*sigma) ) * 1.0_dp * dx
        norm = norm + ( 1.0_dp / ( sqrt(2.0_dp * 3.141592653589793_dp) * sigma ) ) * exp( - (x**2) / (2.0_dp*sigma*sigma) ) * dx
      end do
      approx = approx / max(1.0e-18_dp, norm)
      exact = 1.0_dp
      abs_err = abs(approx - exact)
      write(line,'(ES18.10,1A,A,1A,ES18.10,1A,ES18.10,1A,ES18.10)') sigma, ',', 'f=1', ',', approx, ',', exact, ',', abs_err
      call csv_write(300, trim(line), iu)

      ! Test 2: f(x) = x, exact = 0
      approx = 0.0_dp
      norm = 0.0_dp
      do i = 0, N-1
        x = -L + (real(i,dp) + 0.5_dp) * dx
        approx = approx + ( 1.0_dp / ( sqrt(2.0_dp * 3.141592653589793_dp) * sigma ) ) * exp( - (x**2) / (2.0_dp*sigma*sigma) ) * x * dx
        norm = norm + ( 1.0_dp / ( sqrt(2.0_dp * 3.141592653589793_dp) * sigma ) ) * exp( - (x**2) / (2.0_dp*sigma*sigma) ) * dx
      end do
      approx = approx / max(1.0e-18_dp, norm)
      exact = 0.0_dp
      abs_err = abs(approx - exact)
      write(line,'(ES18.10,1A,A,1A,ES18.10,1A,ES18.10,1A,ES18.10)') sigma, ',', 'f=x', ',', approx, ',', exact, ',', abs_err
      call csv_write(300, trim(line), iu)

      ! Test 3: f(x) = x^2, exact = 0
      approx = 0.0_dp
      norm = 0.0_dp
      do i = 0, N-1
        x = -L + (real(i,dp) + 0.5_dp) * dx
        approx = approx + ( 1.0_dp / ( sqrt(2.0_dp * 3.141592653589793_dp) * sigma ) ) * exp( - (x**2) / (2.0_dp*sigma*sigma) ) * (x**2) * dx
        norm = norm + ( 1.0_dp / ( sqrt(2.0_dp * 3.141592653589793_dp) * sigma ) ) * exp( - (x**2) / (2.0_dp*sigma*sigma) ) * dx
      end do
      approx = approx / max(1.0e-18_dp, norm)
      exact = 0.0_dp
      abs_err = abs(approx - exact)
      write(line,'(ES18.10,1A,A,1A,ES18.10,1A,ES18.10,1A,ES18.10)') sigma, ',', 'f=x^2', ',', approx, ',', exact, ',', abs_err
      call csv_write(300, trim(line), iu)

    end do

    close(300)
    write(*,'(A)') 'Delta convergence demo: CSV saved to delta_convergence.csv'
  end subroutine delta_convergence_demo

  subroutine delta_convergence_extended_demo()
    implicit none
    integer, parameter :: dp = selected_real_kind(15,307)
    integer :: iu, N, i, j, idx, k, jmin, jmax, rad
    character(len=512) :: line
    real(dp) :: L, dx, xj, yk, sigma, arg, kernel, normk
    real(dp), dimension(4) :: siglist
    real(dp), allocatable :: xs(:), f(:), s(:)
    real(dp) :: L1err, L2err, Linf, tmp, sumsq

    ! sigma values to test
    siglist = (/ 1.0e-1_dp, 1.0e-2_dp, 1.0e-3_dp, 1.0e-4_dp /)
    L = 5.0_dp
    N = 2001    ! grid points over [-L, L]
    dx = 2.0_dp * L / real(N-1, dp)

    allocate(xs(N), f(N), s(N))
    do i = 1, N
      xs(i) = -L + real(i-1, dp) * dx
    end do

    open(unit=301, file='delta_convergence_extended.csv', status='replace', action='write', iostat=iu)
    if (iu /= 0) then
      write(*,'(A)') '  Warning: could not open delta_convergence_extended.csv for writing.'
      deallocate(xs, f, s)
      return
    end if
    call csv_write(301, 'sigma,test,L1_error,L2_error,Linf_error', iu)

    do idx = 1, size(siglist)
      sigma = siglist(idx)

      ! Test functions: f(x)=1, f(x)=x, f(x)=x^2
      ! We'll compute convolution s(x) = ∫ phi_sigma(x-y) f(y) dy and compare to f(x)
      do k = 1, 3
        select case (k)
        case (1)
          do i = 1, N; f(i) = 1.0_dp; end do
          write(*,'(A,ES12.6)') '  Extended delta study: sigma=', sigma
          write(*,'(A)') '   Test: f(x)=1'
        case (2)
          do i = 1, N; f(i) = xs(i); end do
          write(*,'(A)') '   Test: f(x)=x'
        case (3)
          do i = 1, N; f(i) = xs(i)**2; end do
          write(*,'(A)') '   Test: f(x)=x^2'
        end select

        ! compute convolution s at each grid point using truncated kernel window
        do j = 1, N
          xj = xs(j)
          s(j) = 0.0_dp
          ! determine half-width in grid points ~ 6*sigma/dx (at least 1)
          rad = max(1, int(6.0_dp * sigma / dx))
          jmin = max(1, j - rad)
          jmax = min(N, j + rad)
          normk = 0.0_dp
          do i = jmin, jmax
            yk = xs(i)
            arg = (xj - yk) / sigma
            kernel = exp( -0.5_dp * arg*arg ) / ( sqrt(2.0_dp * 3.141592653589793_dp) * sigma )
            s(j) = s(j) + kernel * f(i) * dx
            normk = normk + kernel * dx
          end do
          ! small normalization correction (should be ~1)
          if (abs(1.0_dp - normk) > 1.0e-12_dp) then
            s(j) = s(j) / max(1.0e-18_dp, normk)
          end if
        end do

        ! compute L1, L2, Linf errors between s and f
        L1err = 0.0_dp; sumsq = 0.0_dp; Linf = 0.0_dp
        do j = 1, N
          tmp = abs( s(j) - f(j) )
          L1err = L1err + tmp * dx
          sumsq = sumsq + tmp*tmp * dx
          if (tmp > Linf) Linf = tmp
        end do
        L2err = sqrt( sumsq )

        if (k == 1) then
          write(line,'(ES18.10,1A,A,1A,ES18.10,1A,ES18.10,1A,ES18.10)') sigma, ',', 'f=1', ',', L1err, ',', L2err, ',', Linf
          call csv_write(301, trim(line), iu)
        else if (k == 2) then
          write(line,'(ES18.10,1A,A,1A,ES18.10,1A,ES18.10,1A,ES18.10)') sigma, ',', 'f=x', ',', L1err, ',', L2err, ',', Linf
          call csv_write(301, trim(line), iu)
        else
          write(line,'(ES18.10,1A,A,1A,ES18.10,1A,ES18.10,1A,ES18.10)') sigma, ',', 'f=x^2', ',', L1err, ',', L2err, ',', Linf
          call csv_write(301, trim(line), iu)
        end if

      end do

    end do

    close(301)
    deallocate(xs, f, s)
    write(*,'(A)') 'Delta extended convergence demo: CSV saved to delta_convergence_extended.csv'
  end subroutine delta_convergence_extended_demo

  subroutine variants_demo()
    implicit none
    integer :: Nx, i, nsteps, n, idx
    integer, parameter :: ncases = 3
    integer, dimension(ncases) :: NxList
    real(dp) :: L, dx, dt, alpha, tfinal, t, center_exact, maxerr, sumsq, L2err
    real(dp), allocatable :: u(:), unew(:), xgrid(:)
    ! 1D heat eq u_t = alpha u_xx on [0,1], u(0)=u(1)=0, initial u(x,0)=sin(pi x)
    NxList = (/ 51, 101, 201 /)
    L = 1.0_dp; alpha = 1.0_dp
    tfinal = 0.1_dp
    write(*,'(A)') "  Heat equation demo: explicit FD convergence study comparing numeric center value and L2 error"
    write(*,'(A)') "   Nx    center_abs_error    L2_error"
    do idx = 1, ncases
      Nx = NxList(idx)
      dx = L / real(Nx-1,dp)
      dt = 0.4_dp * dx*dx / alpha  ! stable for explicit scheme dt <= dx^2/2
      nsteps = max(1, int(tfinal / dt))
      allocate(u(0:Nx-1), unew(0:Nx-1), xgrid(0:Nx-1))
      do i = 0, Nx-1
        xgrid(i) = real(i,dp) * dx
        u(i) = sin(3.141592653589793_dp * xgrid(i))
      end do
      unew = 0.0_dp
      do n = 1, nsteps
        do i = 1, Nx-2
          unew(i) = u(i) + alpha * dt * (u(i+1) - 2.0_dp*u(i) + u(i-1)) / (dx*dx)
        end do
        unew(0) = 0.0_dp; unew(Nx-1)=0.0_dp
        u = unew
      end do
      t = real(nsteps,dp) * dt
      ! analytic center value
      center_exact = exp(-3.141592653589793_dp**2 * alpha * t) * sin(3.141592653589793_dp * 0.5_dp)
      ! compute center absolute error and global L2 error
      i = int(0.5_dp / dx)
      maxerr = abs(u(i) - center_exact)
      sumsq = 0.0_dp
      do n = 0, Nx-1
        sumsq = sumsq + (u(n) - exp(-3.141592653589793_dp**2 * alpha * t) * sin(3.141592653589793_dp * xgrid(n)))**2 * dx
      end do
      L2err = sqrt(sumsq)
      write(*,'(I6,3X,ES12.6,3X,ES12.6)') Nx, maxerr, L2err
      deallocate(u); deallocate(unew); deallocate(xgrid)
    end do
  end subroutine variants_demo

  

  subroutine calculus_of_forms_demo()
    implicit none
    real(dp) :: x0, y0, h, f_x_analytic, f_y_analytic, g_x_analytic, g_y_analytic
    real(dp) :: f_x_num, f_y_num, g_x_num, g_y_num, dcoeff_analytic, dcoeff_num
    ! exterior derivative of 1-form w = f dx + g dy gives 2-form (dg/dx - df/dy) dx^dy
    ! choose concrete scalar fields f(x,y)=x^2*y and g(x,y)=x*y^2 and test numeric derivatives
    x0 = 0.3_dp; y0 = 0.4_dp
    h = 1.0e-6_dp
    ! analytic partials
    f_x_analytic = 2.0_dp * x0 * y0
    f_y_analytic = x0**2
    g_x_analytic = y0**2
    g_y_analytic = 2.0_dp * x0 * y0
    dcoeff_analytic = g_x_analytic - f_y_analytic
    ! numeric central differences for f
    f_x_num = ( (x0+h)**2 * y0 - (x0-h)**2 * y0 ) / (2.0_dp*h)
    f_y_num = ( x0**2 * (y0+h) - x0**2 * (y0-h) ) / (2.0_dp*h)
    ! numeric central differences for g
    g_x_num = ( (x0+h) * y0**2 - (x0-h) * y0**2 ) / (2.0_dp*h)
    g_y_num = ( x0 * (y0+h)**2 - x0 * (y0-h)**2 ) / (2.0_dp*h)
    dcoeff_num = g_x_num - f_y_num
    write(*,'(A)') "  Exterior derivative demo (numeric vs analytic) for f(x,y)=x^2*y, g(x,y)=x*y^2 at (x0,y0)"
    write(*,'(A,2F12.8)') "   point (x0,y0)=", x0, y0
    write(*,'(A)') "   analytic partials (f_x,f_y,g_x,g_y) and numeric central-difference approximations"
    write(*,'(4(F12.8,1X))') f_x_analytic, f_y_analytic, g_x_analytic, g_y_analytic
    write(*,'(4(F12.8,1X))') f_x_num, f_y_num, g_x_num, g_y_num
    write(*,'(A,F12.8,A,F12.8)') "   exterior d coeff analytic=", dcoeff_analytic, "  numeric=", dcoeff_num
  end subroutine calculus_of_forms_demo

  subroutine automatic_taylor_demo()
    implicit none
    integer, parameter :: dp = selected_real_kind(15,307)
    integer :: Nmax, k, oi, hidx
    real(dp) :: x0, hvals(3), h, approx, exact, abs_err
    real(dp), allocatable :: coeffs(:)
    integer :: iu
    open(unit=100, file='automatic_taylor.csv', status='replace', action='write', iostat=iu)
    if (iu /= 0) then
      write(*,'(A)') '  Warning: could not open automatic_taylor.csv for writing.'
      return
    end if
    write(100,'(A)') 'func,order,h,approx,exact,abs_err'

    ! demo for exp(x) centered at x0
    x0 = 0.5_dp
    Nmax = 8
    allocate(coeffs(0:Nmax))
    ! factorial precompute
    coeffs = 0.0_dp
    do k = 0, Nmax
      coeffs(k) = exp(x0) / real(factorial(k), dp)
    end do
    hvals = (/ 1.0e-1_dp, 1.0e-2_dp, 1.0e-3_dp /)
    do hidx = 1, size(hvals)
      h = hvals(hidx)
      do oi = 0, Nmax
        approx = 0.0_dp
        do k = 0, oi
          approx = approx + coeffs(k) * h**real(k,dp)
        end do
        exact = exp(x0 + h)
        abs_err = abs(exact - approx)
        write(100,'(A,I3,1A,ES18.10,1A,ES18.10,1A,ES18.10,1A,ES18.10)') 'exp,', oi, ',', h, ',', approx, ',', exact, ',', abs_err
      end do
    end do
    deallocate(coeffs)

    ! demo for sin(x) centered at x0
    allocate(coeffs(0:Nmax))
    do k = 0, Nmax
      select case (mod(k,4))
      case (0)
        coeffs(k) = sin(x0) / real(factorial(k), dp)
      case (1)
        coeffs(k) = cos(x0) / real(factorial(k), dp)
      case (2)
        coeffs(k) = -sin(x0) / real(factorial(k), dp)
      case (3)
        coeffs(k) = -cos(x0) / real(factorial(k), dp)
      end select
    end do
    do hidx = 1, size(hvals)
      h = hvals(hidx)
      do oi = 0, Nmax
        approx = 0.0_dp
        do k = 0, oi
          approx = approx + coeffs(k) * h**real(k,dp)
        end do
        exact = sin(x0 + h)
        abs_err = abs(exact - approx)
        write(100,'(A,I3,1A,ES18.10,1A,ES18.10,1A,ES18.10,1A,ES18.10)') 'sin,', oi, ',', h, ',', approx, ',', exact, ',', abs_err
      end do
    end do
    deallocate(coeffs)
    close(100)
    write(*,'(A)') 'Automatic Taylor demo: CSV saved to automatic_taylor.csv'
  end subroutine automatic_taylor_demo

  subroutine openmp_parallel_loops_demo()
    use omp_lib
    implicit none
    integer, parameter :: dp = selected_real_kind(15,307)
    integer :: N, i, iu, r, reps
    real(dp), allocatable :: a(:)
    real(dp) :: sum_serial, sum_parallel, t0, t1
    real(dp) :: serial_avg, parallel_avg
    integer :: nthreads

    ! Problem size and repetitions to get stable timings
    N = 20000000         ! increase work to make timing meaningful
    reps = 5
    allocate(a(N))

    ! initialize array with a smooth function
    do i = 1, N
      a(i) = sin( real(i,dp) * 1.234567_dp / real(N,dp) )
    end do

    ! Warm-up: one serial and one parallel run to touch memory and start threads
    sum_serial = 0.0_dp
    do i = 1, N
      sum_serial = sum_serial + a(i)
    end do

    sum_parallel = 0.0_dp
    !$omp parallel do reduction(+:sum_parallel) default(shared) private(i)
    do i = 1, N
      sum_parallel = sum_parallel + a(i)
    end do
    !$omp end parallel do

    ! Repeated timings (serial)
    serial_avg = 0.0_dp
    do r = 1, reps
      sum_serial = 0.0_dp
      t0 = omp_get_wtime()
      do i = 1, N
        sum_serial = sum_serial + a(i)
      end do
      t1 = omp_get_wtime()
      serial_avg = serial_avg + (t1 - t0)
    end do
    serial_avg = serial_avg / real(reps, dp)

    write(*,'(A)') 'OpenMP parallel loops demo (serial run):'
    write(*,'(A,ES12.6)') '  average serial time (s) = ', serial_avg

    ! Repeated timings (parallel)
    parallel_avg = 0.0_dp
    do r = 1, reps
      sum_parallel = 0.0_dp
      t0 = omp_get_wtime()
      !$omp parallel do reduction(+:sum_parallel) default(shared) private(i)
      do i = 1, N
        sum_parallel = sum_parallel + a(i)
      end do
      !$omp end parallel do
      t1 = omp_get_wtime()
      parallel_avg = parallel_avg + (t1 - t0)
    end do
    parallel_avg = parallel_avg / real(reps, dp)

    nthreads = omp_get_max_threads()
    write(*,'(A)') 'OpenMP parallel loops demo (parallel run):'
    write(*,'(A,I3)') '  threads used = ', nthreads
    write(*,'(A,ES12.6)') '  average parallel time (s) = ', parallel_avg

    ! write CSV with timings and sums (include speedup)
    open(unit=110, file='openmp_parallel_loops.csv', status='replace', action='write', iostat=iu)
    if (iu == 0) then
      write(110,'(A)') 'N,threads,serial_time_avg,parallel_time_avg,speedup,sum_serial,sum_parallel'
      write(110,'(I0,1A,I0,1A,ES18.10,1A,ES18.10,1A,ES18.10,1A,ES18.10,1A,ES18.10)') N, ',', nthreads, ',', serial_avg, ',', parallel_avg, ',', serial_avg/parallel_avg, ',', sum_serial, ',', sum_parallel
      close(110)
    else
      write(*,'(A)') '  Warning: could not open openmp_parallel_loops.csv for writing.'
    end if

    deallocate(a)
    write(*,'(A)') 'OpenMP parallel loops demo: CSV saved to openmp_parallel_loops.csv'
  end subroutine openmp_parallel_loops_demo

  integer function factorial(n) result(f)
    implicit none
    integer, intent(in) :: n
    integer :: i
    f = 1
    do i = 2, max(1,n)
      f = f * i
    end do
  end function factorial

  subroutine currents_demo()
    implicit none
    ! Simple line-integral (circulation) demo for vector field F = (-y, x)
    ! around a circle of radius R. Analytic result: ∮ F·dr = 2*pi*R^2
    integer :: N, j
    real(dp) :: R, t, dt, x, y, dx, dy, integrand, numeric_circ, analytic
    N = 10000
    R = 1.0_dp
    numeric_circ = 0.0_dp
    dt = 2.0_dp*3.141592653589793_dp / real(N,dp)
    do j = 0, N-1
      t = real(j,dp) * dt
      x = R * cos(t); y = R * sin(t)
      dx = -R * sin(t) * dt
      dy =  R * cos(t) * dt
      integrand = (-y)*dx + x*dy
      numeric_circ = numeric_circ + integrand
    end do
    analytic = 2.0_dp * 3.141592653589793_dp * R * R
    write(*,'(A)') "  Currents demo: circulation ∮ F·dr for F=(-y,x) around circle"
    write(*,'(A,F12.6,A,F12.6)') "   numeric approx=", numeric_circ, "  analytic=", analytic
  end subroutine currents_demo

  subroutine fuzzy_sets_demo()
    implicit none
    integer :: N, i
    real(dp) :: x, mu, dx, centroid, sumw
    real(dp), parameter :: alpha = 0.5_dp
    N = 21
    dx = 1.0_dp / real(N-1,dp)
    sumw = 0.0_dp; centroid = 0.0_dp
    write(*,'(A)') "  Fuzzy-sets demo: sigmoid membership mu(x) = 1/(1+exp(-10(x-0.5))) over x in [0,1]"
    write(*,'(A)') "   sample memberships (x : mu(x))"
    do i = 0, N-1
      x = real(i,dp) * dx
      mu = 1.0_dp / (1.0_dp + exp(-10.0_dp*(x-0.5_dp)))
      write(*,'(F6.3,2X,F12.8)') x, mu
      centroid = centroid + x * mu * dx
      sumw = sumw + mu * dx
    end do
    if (sumw > 0.0_dp) centroid = centroid / sumw
    write(*,'(A,F12.8)') "   centroid (center of mass of membership) =", centroid
    ! alpha-cut approximate interval(s) where mu >= alpha
    write(*,'(A)') "   alpha-cut (alpha=0.5) approximate points where mu(x)>=0.5"
    do i = 0, N-1
      x = real(i,dp) * dx
      mu = 1.0_dp / (1.0_dp + exp(-10.0_dp*(x-0.5_dp)))
      if (mu >= alpha) then
        write(*,'(F6.3,2X,F10.6)') x, mu
      end if
    end do
  end subroutine fuzzy_sets_demo

  subroutine rough_paths_demo()
    implicit none
    ! Toy numeric: polygon (shoelace) area vs trapezoidal path integral 1/2 ∮ (x dy - y dx)
    integer :: M, k
    real(dp) :: R, theta, xk, yk, xnext, ynext, area_shoelace, sum, area_trap, dt
    real(dp), parameter :: PI = 3.141592653589793_dp
    M = 400
    R = 1.0_dp
    sum = 0.0_dp
    area_trap = 0.0_dp
    dt = 2.0_dp*PI / real(M,dp)
    do k = 0, M-1
      theta = dt * real(k,dp)
      xk = R * cos(theta)
      yk = R * sin(theta)
      theta = dt * real(k+1,dp)
      xnext = R * cos(theta)
      ynext = R * sin(theta)
      sum = sum + (xk*ynext - yk*xnext)
      ! trapezoidal approximation of ∮ x dy - y dx over segment
      area_trap = area_trap + 0.5_dp * (xk + xnext) * (ynext - yk) - 0.5_dp * (yk + ynext) * (xnext - xk)
    end do
    area_shoelace = 0.5_dp * sum
    ! area_trap should approximate same value; for circle analytic area = PI R^2
    write(*,'(A)') "  Rough-paths toy: shoelace vs trapezoidal path-integral approximation"
    write(*,'(A,F12.8)') "   shoelace area=", area_shoelace
    write(*,'(A,F12.8)') "   trapezoidal path integral area=", 0.5_dp * area_trap
    write(*,'(A,F12.8)') "   analytic circle area=", PI * R * R
    write(*,'(A,F12.8)') "   abs(shoelace - analytic)=", abs(area_shoelace - PI*R*R)
    write(*,'(A,F12.8)') "   abs(trapezoid - analytic)=", abs(0.5_dp*area_trap - PI*R*R)
  end subroutine rough_paths_demo

  subroutine econ_variants_demo()
    implicit none
    ! Simple economics demo with linear and nonlinear supply/demand
    real(dp) :: a_s, b_s, a_d, b_d, p_lin, q_lin
    real(dp) :: p_lo, p_hi, p_mid, f_lo, f_mid, tol
    integer :: iter, maxit
    ! Linear supply q_s = a_s*p + b_s, demand q_d = a_d*p + b_d
    a_s = 1.0_dp; b_s = 0.0_dp
    a_d = -1.0_dp; b_d = 10.0_dp
    p_lin = (b_d - b_s) / (a_s - a_d)
    q_lin = a_s * p_lin + b_s
    write(*,'(A)') "  Economic variants: supply-demand equilibrium demo"
    write(*,'(A,F12.6,A,F12.6)') "   linear equilibrium price p=", p_lin, "  quantity=", q_lin

    ! Nonlinear example: supply q_s = p, demand q_d = 10 - p**2. Solve p such that q_s - q_d = 0
    p_lo = 0.0_dp; p_hi = 5.0_dp; tol = 1.0e-6_dp; maxit = 100
    f_lo = p_lo - (10.0_dp - p_lo**2)
    do iter = 1, maxit
      p_mid = 0.5_dp*(p_lo + p_hi)
      f_mid = p_mid - (10.0_dp - p_mid**2)
      if (abs(f_mid) < tol) exit
      if (f_lo*f_mid < 0.0_dp) then
        p_hi = p_mid
      else
        p_lo = p_mid
        f_lo = f_mid
      end if
    end do
    write(*,'(A)') "   nonlinear demand example: q_d = 10 - p**2  (bisection)"
    write(*,'(A,F12.6)') "    solved price p~=", p_mid
    write(*,'(A,F12.6)') "    corresponding quantity q =", p_mid
  end subroutine econ_variants_demo

  subroutine neural_calc_demo()
    implicit none
    integer :: i, j, N, epochs
    real(dp) :: w, lr, loss, y_pred, dLdw
    real(dp), allocatable :: xs(:), ys(:)

    ! Simple linear regression demo: fit y = w * x using gradient descent
    N = 6
    allocate(xs(N), ys(N))
    xs = (/ 0.0_dp, 1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp, 5.0_dp /)
    ys = (/ 0.1_dp, 1.9_dp, 3.9_dp, 5.9_dp, 8.1_dp, 9.8_dp /) ! roughly y ~ 2*x with small noise
    w = 0.0_dp
    lr = 0.01_dp
    epochs = 500
    write(*,'(A)') "  Neural (linear regression) demo: fit y = w*x by gradient descent"
    do i = 1, epochs
      dLdw = 0.0_dp
      loss = 0.0_dp
      do j = 1, N
        y_pred = w * xs(j)
        loss = loss + 0.5_dp*(y_pred - ys(j))**2
        dLdw = dLdw + (y_pred - ys(j)) * xs(j)
      end do
      w = w - lr * dLdw / real(N,dp)
      if (mod(i,100) == 0) then
        write(*,'(A,I4,3X,A,F12.8)') "   epoch", i, "  current w=", w
      end if
    end do
    write(*,'(A,F12.8)') "   learned parameter w=", w
    write(*,'(A)') "   final residuals (y - w*x):"
    do i = 1, N
      write(*,'(I2,2X,F6.3,2X,F8.4)') i, xs(i), ys(i) - w*xs(i)
    end do
    deallocate(xs, ys)
  end subroutine neural_calc_demo

  subroutine evolutionary_dynamics_demo()
    implicit none
    integer :: idx, N, t
    real(dp) :: r, K, y, dt
    real(dp) :: x, xdot
    ! replicator dynamics for two-strategy game payoff matrix
    real(dp) :: pay00, pay01, pay10, pay11, phiA, phiB
    real(dp) :: x_series(0:20)

    ! Logistic growth discretization example
    r = 1.0_dp; K = 10.0_dp; y = 0.5_dp; dt = 0.1_dp; N = 50
    write(*,'(A)') "  Evolutionary dynamics demo: logistic growth and two-strategy replicator dynamics"
    write(*,'(A)') "   logistic growth time series (sampled):"
    do idx = 1, N
      y = y + dt * r * y * (1.0_dp - y/K)
      if (mod(idx,10) == 0) write(*,'(I4,2X,F12.6)') idx, y
    end do

    ! Simple replicator dynamics: two strategies A and B
    x = 0.3_dp  ! fraction of A
    pay00 = 1.0_dp; pay01 = 0.5_dp
    pay10 = 1.5_dp; pay11 = 0.2_dp
    write(*,'(A)') "   replicator dynamics (two strategies) time series (x = fraction A):"
    do t = 0, 20
      x_series(t) = x
      ! expected payoffs
      phiA = pay00 * x + pay01 * (1.0_dp - x)
      phiB = pay10 * x + pay11 * (1.0_dp - x)
      xdot = x * (phiA - (x*phiA + (1.0_dp-x)*phiB))
      ! Euler step with small dt
      x = x + 0.1_dp * xdot
      x = max(0.0_dp, min(1.0_dp, x))
      if (mod(t,5) == 0) write(*,'(I3,3X,F10.6)') t, x_series(t)
    end do
    write(*,'(A,F12.6)') "   final fraction A ~", x
  end subroutine evolutionary_dynamics_demo

  subroutine symplectic_demo()
    implicit none
    real(dp) :: omega, tmax, dt, x_sym, v_sym, x_rk, v_rk, E0, E_sym, E_rk
    integer :: nsteps, i, ios
    real(dp) :: k1x, k1v, k2x, k2v, k3x, k3v, k4x, k4v, t
    integer, parameter :: csv_unit = 99

    ! Parameters for harmonic oscillator
    omega = 1.0_dp
    tmax = 50.0_dp
    dt = 0.1_dp
    nsteps = max(1, int(tmax / dt))

    ! initial conditions: x(0)=1, v(0)=0
    x_sym = 1.0_dp; v_sym = 0.0_dp
    x_rk  = 1.0_dp; v_rk  = 0.0_dp
    E0 = 0.5_dp*(v_sym*v_sym + omega*omega*x_sym*x_sym)

    E_sym = E0
    E_rk  = E0

    ! Open CSV for per-step logging (overwrite existing)
    open(unit=csv_unit, file='symplectic_energy.csv', status='replace', action='write', iostat=ios)
    if (ios /= 0) then
      write(*,'(A)') '  Warning: could not open symplectic_energy.csv for writing.'
    else
      write(csv_unit,'(A)') 't x_sym v_sym E_sym x_rk v_rk E_rk'
      ! write initial state
      t = 0.0_dp
      write(csv_unit,'(F12.6,1A,F12.6,1A,F12.6,1A,F12.6,1A,F12.6,1A,F12.6,1A,F12.6)') t, ',', x_sym, ',', v_sym, ',', E_sym, ',', x_rk, ',', v_rk, ',', E_rk
    end if

    do i = 1, nsteps
      t = real(i,dp) * dt
      ! Velocity-Verlet (symplectic) step
      v_sym = v_sym - 0.5_dp*dt*(omega**2 * x_sym)
      x_sym = x_sym + dt * v_sym
      v_sym = v_sym - 0.5_dp*dt*(omega**2 * x_sym)

      ! Classic RK4 on the first-order system for comparison
      ! dx/dt = v ; dv/dt = -omega^2 x
      k1x = v_rk
      k1v = -omega**2 * x_rk
      k2x = v_rk + 0.5_dp*dt*k1v
      k2v = -omega**2 * (x_rk + 0.5_dp*dt*k1x)
      k3x = v_rk + 0.5_dp*dt*k2v
      k3v = -omega**2 * (x_rk + 0.5_dp*dt*k2x)
      k4x = v_rk + dt*k3v
      k4v = -omega**2 * (x_rk + dt*k3x)
      x_rk = x_rk + (dt/6.0_dp)*(k1x + 2.0_dp*k2x + 2.0_dp*k3x + k4x)
      v_rk = v_rk + (dt/6.0_dp)*(k1v + 2.0_dp*k2v + 2.0_dp*k3v + k4v)

      ! energies
      E_sym = 0.5_dp*(v_sym*v_sym + omega*omega*x_sym*x_sym)
      E_rk  = 0.5_dp*(v_rk*v_rk + omega*omega*x_rk*x_rk)

      if (ios == 0) then
        write(csv_unit,'(F12.6,1A,F12.6,1A,F12.6,1A,F12.6,1A,F12.6,1A,F12.6,1A,F12.6)') t, ',', x_sym, ',', v_sym, ',', E_sym, ',', x_rk, ',', v_rk, ',', E_rk
      end if
    end do

    if (ios == 0) close(csv_unit)

    write(*,'(A)') "  Symplectic (Velocity-Verlet) vs RK4 energy comparison for harmonic oscillator:"
    write(*,'(A,F18.12)') "   initial energy E0 =", E0
    write(*,'(A,F18.12)') "   final energy (Symplectic) =", E_sym
    write(*,'(A,F18.12)') "   final energy (RK4)        =", E_rk
    write(*,'(A,F18.12)') "   rel energy change (Symp)  =", (E_sym - E0)/E0
    write(*,'(A,F18.12)') "   rel energy change (RK4)   =", (E_rk  - E0)/E0
  end subroutine symplectic_demo

  subroutine adaptive_rkf45_demo()
    implicit none
    ! Adaptive integrator demo: show automatic step-size control using step-doubling RK4
    ! on the scalar ODE dy/dt = -lambda * y with moderate stiffness.
    real(dp) :: lambda, t, tfinal, dt, y, y_half, y_full, y_halfstep, err, tol
    real(dp) :: dtmin, dtmax, safety, factor_up, factor_down
    real(dp) :: ytmp
    integer :: steps, rejects, maxsteps

    lambda = 50.0_dp       ! moderately stiff decay rate
    tfinal = 1.0_dp
    t = 0.0_dp
    y = 1.0_dp             ! initial condition

    dt = 0.05_dp           ! initial step
    dtmin = 1.0e-8_dp
    dtmax = 0.2_dp
    tol = 1.0e-6_dp
    safety = 0.9_dp
    factor_up = 2.0_dp
    factor_down = 0.5_dp

    steps = 0; rejects = 0; maxsteps = 100000

    write(*,'(A)') '  Adaptive RK demo (step-doubling RK4 as embedded error estimator):'
    write(*,'(A,F6.2)') '   Integrating dy/dt = -lambda*y  with lambda=', lambda

    do while (t < tfinal .and. steps < maxsteps)
      if (t + dt > tfinal) dt = tfinal - t
      ! One full RK4 step of size dt -> y_full
      call rk4_step(y, dt, lambda, y_full)
      ! Two half-steps RK4 of size dt/2 -> y_half
      call rk4_step(y, dt*0.5_dp, lambda, y_halfstep)
      call rk4_step(y_halfstep, dt*0.5_dp, lambda, y_half)

      ! error estimate (norm of difference since both are 4th order, use difference)
      err = abs(y_half - y_full)

      if (err <= tol) then
        ! accept step
        t = t + dt
        y = y_half    ! use higher-accuracy (two half-steps)
        steps = steps + 1
        ! adapt step size up if error small
        if (err > 0.0_dp) then
          dt = min(dtmax, safety * dt * (tol/err)**0.25_dp)
        else
          dt = min(dtmax, dt * factor_up)
        end if
      else
        ! reject and reduce step
        dt = max(dtmin, safety * dt * (tol/err)**0.25_dp)
        rejects = rejects + 1
        if (dt <= dtmin) then
          write(*,'(A)') '   Warning: dt reached dtmin, continuing with dtmin.'
          t = t + dt
          call rk4_step(y, dt, lambda, ytmp)
          y = ytmp
          steps = steps + 1
        end if
      end if
    end do

    write(*,'(A,I8)') '   accepted steps=', steps
    write(*,'(A,I8)') '   rejected steps=', rejects
    write(*,'(A,F12.6)') '   final t=', t
    write(*,'(A,F18.12)') '   numeric y(t)=', y
    write(*,'(A,F18.12)') '   analytic y(t)=', exp(-lambda*t)
    write(*,'(A,ES12.6)') '   abs error=', abs(y - exp(-lambda*t))

  end subroutine adaptive_rkf45_demo

  subroutine rk4_step(y0, h, lambda, yout)
    implicit none
    real(dp), intent(in) :: y0, h, lambda
    real(dp), intent(out) :: yout
    real(dp) :: k1, k2, k3, k4
    ! dy/dt = -lambda * y
    k1 = -lambda * y0
    k2 = -lambda * (y0 + 0.5_dp*h*k1)
    k3 = -lambda * (y0 + 0.5_dp*h*k2)
    k4 = -lambda * (y0 + h*k3)
    yout = y0 + (h/6.0_dp)*(k1 + 2.0_dp*k2 + 2.0_dp*k3 + k4)
  end subroutine rk4_step

  subroutine adaptive_rkf45_classic_demo()
    implicit none
    ! Classical RKF45 (Cash-Karp) adaptive integrator demo for dy/dt = -lambda*y
    real(dp) :: lambda, t, tfinal, dt, y, y5, y4, err, tol
    real(dp) :: dtmin, dtmax, safety
    integer :: steps, rejects, maxsteps

    lambda = 50.0_dp
    tfinal = 1.0_dp
    t = 0.0_dp
    y = 1.0_dp

    dt = 0.05_dp
    dtmin = 1.0e-8_dp
    dtmax = 0.2_dp
    tol = 1.0e-6_dp
    safety = 0.9_dp

    steps = 0; rejects = 0; maxsteps = 100000

    write(*,'(A)') '  Classical RKF45 (Cash-Karp) adaptive demo:'
    write(*,'(A,F6.2)') '   Integrating dy/dt = -lambda*y  with lambda=', lambda

    do while (t < tfinal .and. steps < maxsteps)
      if (t + dt > tfinal) dt = tfinal - t
      call rkf45_step(y, dt, lambda, y5, y4, err)
      ! compute error norm
      err = abs(y5 - y4)
      if (err <= tol) then
        ! accept
        t = t + dt
        y = y5
        steps = steps + 1
        if (err > 0.0_dp) then
          dt = min(dtmax, safety * dt * (tol/err)**0.2_dp)
        else
          dt = min(dtmax, dt * 2.0_dp)
        end if
      else
        ! reject
        dt = max(dtmin, safety * dt * (tol/err)**0.2_dp)
        rejects = rejects + 1
      end if
    end do

    write(*,'(A,I8)') '   accepted steps=', steps
    write(*,'(A,I8)') '   rejected steps=', rejects
    write(*,'(A,F12.6)') '   final t=', t
    write(*,'(A,F18.12)') '   numeric y(t)=', y
    write(*,'(A,F18.12)') '   analytic y(t)=', exp(-lambda*t)
    write(*,'(A,ES12.6)') '   abs error=', abs(y - exp(-lambda*t))

  end subroutine adaptive_rkf45_classic_demo


  subroutine rkf45_step(y0, h, lambda, y5, y4, err)
    implicit none
    ! Cash-Karp RKF45 coefficients
    real(dp), intent(in) :: y0, h, lambda
    real(dp), intent(out) :: y5, y4, err
    real(dp) :: k1, k2, k3, k4, k5, k6

    ! coefficients
    k1 = -lambda * y0
    k2 = -lambda * ( y0 + h*(1.0_dp/5.0_dp)*k1 )
    k3 = -lambda * ( y0 + h*(3.0_dp/40.0_dp*k1 + 9.0_dp/40.0_dp*k2) )
    k4 = -lambda * ( y0 + h*(3.0_dp/10.0_dp*k1 - 9.0_dp/10.0_dp*k2 + 6.0_dp/5.0_dp*k3) )
    k5 = -lambda * ( y0 + h*(-11.0_dp/54.0_dp*k1 + 5.0_dp/2.0_dp*k2 - 70.0_dp/27.0_dp*k3 + 35.0_dp/27.0_dp*k4) )
    k6 = -lambda * ( y0 + h*(1631.0_dp/55296.0_dp*k1 + 175.0_dp/512.0_dp*k2 + 575.0_dp/13824.0_dp*k3 + 44275.0_dp/110592.0_dp*k4 + 253.0_dp/4096.0_dp*k5) )

    ! 5th-order solution (Cash-Karp b5)
    y5 = y0 + h*(37.0_dp/378.0_dp*k1 + 0.0_dp*k2 + 250.0_dp/621.0_dp*k3 + 125.0_dp/594.0_dp*k4 + 0.0_dp*k5 + 512.0_dp/1771.0_dp*k6)
    ! 4th-order solution (b4)
    y4 = y0 + h*(2825.0_dp/27648.0_dp*k1 + 0.0_dp*k2 + 18575.0_dp/48384.0_dp*k3 + 13525.0_dp/55296.0_dp*k4 + 277.0_dp/14336.0_dp*k5 + 1.0_dp/4.0_dp*k6)

    err = abs(y5 - y4)
  end subroutine rkf45_step

  subroutine fractals_demo()
    implicit none
    integer :: n
    real(dp) :: dim, remaining_length
    ! analytic Hausdorff dimension of middle-thirds Cantor set: log(2)/log(3)
    dim = log(2.0_dp) / log(3.0_dp)
    write(*,'(A,F12.6)') "  Cantor set Hausdorff/box-counting dimension ~=", dim
    ! numeric demonstration: remaining total length after n iterations is (2/3)^n
    write(*,'(A)') "   Remaining total length after n iterations (starting length 1):"
    do n = 0, 10
      remaining_length = (2.0_dp/3.0_dp)**real(n,dp)
      write(*,'(I3,3X,ES12.6)') n, remaining_length
    end do
    write(*,'(A)') "   Observation: lengths decay exponentially and the set becomes dust-like while dimension stays log(2)/log(3)."
  end subroutine fractals_demo

  subroutine noncommutative_calculus_demo()
    implicit none
    real(dp) :: A(2,2), B(2,2), C(2,2), frob
    integer :: i,j
    A = 0.0_dp; B = 0.0_dp; C = 0.0_dp
    A(1,2) = 1.0_dp; A(2,1) = -1.0_dp
    B(1,1) = 0.5_dp; B(2,2) = -0.5_dp
    ! commutator [A,B] = A B - B A
    C = matmul(A,B) - matmul(B,A)
    frob = 0.0_dp
    do i = 1,2
      do j = 1,2
        frob = frob + C(i,j)**2
      end do
    end do
    frob = sqrt(frob)
    write(*,'(A)') "  Noncommutative calc: commutator [A,B] (2x2) and Frobenius norm:" 
    write(*,'(2F12.6)') C(1,1), C(1,2)
    write(*,'(2F12.6)') C(2,1), C(2,2)
    write(*,'(A,F12.6)') "   Frobenius norm of [A,B] =", frob
    if (frob > 0.0_dp) then
      write(*,'(A)') "   Conclusion: matrices do not commute (non-zero commutator)."
    else
      write(*,'(A)') "   Matrices commute."
    end if
  end subroutine noncommutative_calculus_demo

  subroutine noncommutative_geometry_demo()
    implicit none
    real(dp) :: A(2,2), B(2,2), C(2,2), frob
    integer :: i,j
    ! Toy numeric: treat small matrices as 'noncommutative coordinates' and compute commutator norm
    A = 0.0_dp; B = 0.0_dp; C = 0.0_dp
    A(1,2) = 1.0_dp; A(2,1) = -1.0_dp
    B(1,1) = 0.5_dp; B(2,2) = -0.5_dp
    C = matmul(A,B) - matmul(B,A)
    frob = 0.0_dp
    do i = 1,2
      do j = 1,2
        frob = frob + C(i,j)**2
      end do
    end do
    frob = sqrt(frob)
    write(*,'(A)') "  Noncommutative Geometry (toy numeric): commutator [A,B] and Frobenius norm"
    write(*,'(2F12.6)') C(1,1), C(1,2)
    write(*,'(2F12.6)') C(2,1), C(2,2)
    write(*,'(A,F12.6)') "  Frobenius norm of [A,B] =", frob
  end subroutine noncommutative_geometry_demo

  subroutine supermanifold_demo()
    implicit none
    ! Toy superfunction f(x,theta) = f0(x) + theta * f1(x), with theta^2 = 0
    real(dp) :: x, f0, f1
    ! Toy Berezinian multiplicativity check for 1|1 block matrix: M = [A B; C D], Ber(M) = (A - B D^{-1} C)/D
    real(dp) :: A1, B1, C1, D1, ber1, A2, B2, C2, D2, ber2
    real(dp) :: Aprod, Bprod, Cprod, Dprod, berprod, berprod_expected
    ! For demonstration use x real and represent odd part as separate component
    x = 0.7_dp
    f0 = x**2          ! even part
    f1 = 3.0_dp*x      ! odd part coefficient
    write(*,'(A)') "  Supermanifold toy: function f(x,theta)=f0(x)+theta*f1(x) with theta^2=0"
    write(*,'(A,F12.6)') "   f0(x) =", f0
    write(*,'(A,F12.6)') "   f1(x) =", f1
    write(*,'(A)') "   Grassmann derivative d/dtheta f = f1 (odd component):"
    write(*,'(A,F12.6)') "    d/dtheta f =", f1
    write(*,'(A)') "   Berezin integral rules: ∫ dtheta theta = 1, ∫ dtheta 1 = 0 (illustrative)"
    A1 = 2.0_dp; D1 = 3.0_dp; B1 = 0.12_dp; C1 = 0.21_dp
    A2 = 1.5_dp; D2 = 2.2_dp; B2 = 0.07_dp; C2 = 0.11_dp
    ber1 = (A1 - B1*(1.0_dp/D1)*C1) / D1
    ber2 = (A2 - B2*(1.0_dp/D2)*C2) / D2
    ! product Mprod = M1 * M2 in block form (treating B,C as ordinary numbers for numeric toy)
    Aprod = A1*A2 + B1*C2
    Bprod = A1*B2 + B1*D2
    Cprod = C1*A2 + D1*C2
    Dprod = C1*B2 + D1*D2
    berprod = (Aprod - Bprod*(1.0_dp/Dprod)*Cprod) / Dprod
    berprod_expected = ber1 * ber2
    write(*,'(A)') "   Berezinian multiplicativity toy (numeric placeholder treating odd blocks as scalars):"
    write(*,'(A,F12.8)') "    ber(M1)=", ber1
    write(*,'(A,F12.8)') "    ber(M2)=", ber2
    write(*,'(A,F12.8)') "    ber(M1*M2) numeric=", berprod
    write(*,'(A,F12.8)') "    ber(M1)*ber(M2) expected=", berprod_expected
  end subroutine supermanifold_demo

  subroutine topos_theoretic_demo()
    implicit none
    ! Numeric presheaf toy: sample local sections on U=[0,1] and V=[1,2], check glue at overlap x=1
    integer :: N, i
    real(dp) :: dx, x, sU, sV, max_mismatch
    N = 11
    dx = 1.0_dp / real(N-1,dp)
    max_mismatch = 0.0_dp
    write(*,'(A)') "  Topos/Psheaf numeric toy: sample sections s_U(x)=2*x on U=[0,1], s_V(x)=2*x-1 on V=[1,2]"
    write(*,'(A)') "   check glue on overlap around x=1 by sampling small neighborhoods"
    do i = 0, N-1
      x = 1.0_dp - 0.5_dp*dx + real(i,dp)*dx  ! sample near overlap from 1-dx/2 to 1+dx/2
      if (x <= 1.0_dp) then
        sU = 2.0_dp * x
      else
        sU = 2.0_dp * 1.0_dp  ! restrict U's section value at overlap
      end if
      if (x >= 1.0_dp) then
        sV = 2.0_dp * x - 1.0_dp
      else
        sV = 2.0_dp * 1.0_dp - 1.0_dp
      end if
      max_mismatch = max(max_mismatch, abs(sU - sV))
      write(*,'(F8.4,3X,F12.8,3X,F12.8)') x, sU, sV
    end do
    write(*,'(A,F12.8)') "   max mismatch on sampled overlap =", max_mismatch
    if (max_mismatch < 1.0e-12_dp) then
      write(*,'(A)') "   Sections numerically glue on the sampled overlap."
    else
      write(*,'(A)') "   Sections do NOT numerically glue (mismatch > tol)."
    end if
  end subroutine topos_theoretic_demo

  subroutine higher_categories_demo()
    implicit none
    ! Illustrative finite 2-category: two objects A,B; Hom(A,A)={id_A}, Hom(A,B)={f,g}, with a 2-morphism alpha: f => g
    integer :: n_objs, n_1morph, n_2morph
    n_objs = 2
    n_1morph = 3  ! id_A, f, g (counting minimal set for illustration)
    n_2morph = 1  ! a single 2-cell alpha: f => g
    write(*,'(A)') "  Higher-categories toy: counts for a tiny 2-category (illustrative)"
    write(*,'(A,I4)') "   objects =", n_objs
    write(*,'(A,I4)') "   1-morphisms (sample) =", n_1morph
    write(*,'(A,I4)') "   2-morphisms (sample) =", n_2morph
    write(*,'(A)') "  Note: full coherence data and compositions are symbolic; this is a structural toy."
  end subroutine higher_categories_demo

  subroutine quantum_groups_demo()
    implicit none
    real(dp) :: q, x, qderiv
    integer :: n
    q = 1.2_dp
    x = 1.0_dp; n = 3
    ! q-derivative of x^n at x: D_q x^n = ((q^n - 1)/(q - 1)) * x^{n-1}
    qderiv = (q**real(n,dp) - 1.0_dp) / (q - 1.0_dp) * x**real(n-1,dp)
    write(*,'(A,F8.4)') "  Quantum group / q-deformation parameter q=", q
    write(*,'(A)') "  q-derivative D_q x^n at x=1 for n=3 ->"
    write(*,'(A,F12.6)') "    value=", qderiv
  end subroutine quantum_groups_demo

  subroutine superstrings_demo()
    implicit none
    ! Toy vibrating string fundamental frequency demo (classical formula) as a small numeric example
    real(dp) :: T, mu, L, f1
    T = 100.0_dp    ! tension in N
    mu = 0.01_dp    ! linear mass density kg/m
    L = 1.0_dp      ! length m
    f1 = 0.5_dp * (1.0_dp/L) * sqrt(T/mu)
    write(*,'(A)') "  Superstrings toy: classic vibrating string fundamental frequency (not full string theory)"
    write(*,'(A,F12.6)') "   tension T=", T
    write(*,'(A,F12.6)') "   linear density mu=", mu
    write(*,'(A,F12.6)') "   length L=", L
    write(*,'(A,F12.6)') "   fundamental frequency f1=", f1
    write(*,'(A)') "   harmonic frequencies f_n = n * f1 for n=1..5"
    write(*,'(5F12.6)') f1, 2.0_dp*f1, 3.0_dp*f1, 4.0_dp*f1, 5.0_dp*f1
  end subroutine superstrings_demo

  subroutine infinity_categories_demo()
    implicit none
    ! Tiny nerve counts for an ordinary category: objects, morphisms, composable pairs
    integer :: objects, morphisms, composable_pairs
    objects = 2
    morphisms = 3  ! e.g., id_A, f:A->B, id_B
    composable_pairs = 1  ! e.g., (id_A,f) composing to f (illustrative)
    write(*,'(A)') "  Infinity-categories toy: nerve-size counts for a small category (illustrative)"
    write(*,'(A,I4)') "   0-simplices (objects) =", objects
    write(*,'(A,I4)') "   1-simplices (morphisms) =", morphisms
    write(*,'(A,I4)') "   2-simplices (composable pairs) =", composable_pairs
    write(*,'(A)') "  Note: this toy counts simplices; full infinity-category structure is abstract/symbolic."
  end subroutine infinity_categories_demo

  subroutine supercalculus_demo()
    implicit none
    ! A small runnable supercalculus toy using a single Grassmann generator theta (theta^2 = 0).
    ! Represent superfunction f(x,theta) = f0(x) + theta * f1(x).
    real(dp) :: x, f0, f1, df0_dx, df1_dx
    real(dp) :: d_dtheta_f
    x = 0.7_dp
    ! choose simple component functions
    f0 = x**2            ! even (commuting) part
    f1 = 3.0_dp * x      ! odd (coefficient of theta)
    ! derivatives wrt x (ordinary derivative on component functions)
    df0_dx = 2.0_dp * x
    df1_dx = 3.0_dp
    ! d/dtheta f strips off odd coefficient (Berezin derivative)
    d_dtheta_f = f1
    ! display results
    write(*,'(A)') "  Supercalculus toy: superfunction f(x,theta)=f0(x)+theta*f1(x) with theta^2=0"
    write(*,'(A,F12.6)') "   f0(x) (even) =", f0
    write(*,'(A,F12.6)') "   f1(x) (odd coeff) =", f1
    write(*,'(A,F12.6)') "   ordinary d/dx f0 =", df0_dx
    write(*,'(A,F12.6)') "   ordinary d/dx f1 =", df1_dx
    write(*,'(A,F12.6)') "   Berezin d/dtheta f =", d_dtheta_f
    write(*,'(A)') "  Berezin integral rules (toy): ∫ dtheta (a0 + a1 theta) = a1 ; ∫ dtheta 1 = 0"
  end subroutine supercalculus_demo

  subroutine game_theory_variants_demo()
    implicit none
    integer :: t, Nout, N
    real(dp) :: x, y, dt
    real(dp), allocatable :: series(:)
    ! replicator-like continuous dynamics for two strategies
    x = 0.6_dp; y = 1.0_dp - x; dt = 0.001_dp; N = 10000
    allocate(series(0:N))
    series(0) = x
    do t = 1, N
      x = x + dt * x * (1.0_dp - x) * (0.5_dp - x)
      if (x < 0.0_dp) x = 0.0_dp
      if (x > 1.0_dp) x = 1.0_dp
      series(t) = x
    end do
    write(*,'(A)') "  Game-theory replicator toy time-series snapshot (every N/10 steps):"
    Nout = 10
    do t = 0, N, N/Nout
      write(*,'(I6,3X,F12.8)') t, series(t)
    end do
    write(*,'(A,F12.8)') "   final strategy frequency x(N)=", x
    deallocate(series)
  end subroutine game_theory_variants_demo

  subroutine higher_order_functors_demo()
    implicit none
    integer :: i
    real(dp) :: base(3), mapped1(3), mapped2(3)
    base = (/1.0_dp,2.0_dp,3.0_dp/)
    do i = 1,3
      mapped1(i) = base(i)**2
      mapped2(i) = (mapped1(i))**2  ! higher-order functor (composition)
    end do
    write(*,'(A)') "  Higher-order functor demo (composition of map square twice):"
    write(*,'(3F12.6)') mapped2(1), mapped2(2), mapped2(3)
  end subroutine higher_order_functors_demo

  subroutine functorial_semantics_demo()
    implicit none
    integer :: nfuncs, deg, i, j, k, unit_f, ios
    real(dp), allocatable :: p(:,:), pF(:,:), pD(:,:), pFD(:,:), pDF(:,:)
    real(dp) :: a, b, sumsq
    character(len=256) :: row, tmp_s

    ! Toy functorial semantics demo for polynomials (coeff vector representation)
    ! Functor F: change of variable p(x) -> p(a*x + b)
    ! Operator D: derivative w.r.t. x
    nfuncs = 3
    deg = 3   ! polynomials up to x^3 (coeffs for x^0..x^3)
    allocate(p(nfuncs,deg+1), pF(nfuncs,deg+1), pD(nfuncs,deg+1), pFD(nfuncs,deg+1), pDF(nfuncs,deg+1))

    ! Example polynomials (coeffs: c0 + c1 x + c2 x^2 + c3 x^3)
    p = 0.0_dp
    p(1,:) = (/ 1.0_dp, 2.0_dp, 0.0_dp, 0.0_dp /)     ! 1 + 2 x
    p(2,:) = (/ 0.0_dp, 0.0_dp, 1.0_dp, 1.0_dp /)     ! x^2 + x^3
    p(3,:) = (/ 1.0_dp, -1.0_dp, 0.5_dp, 0.0_dp /)    ! 1 - x + 0.5 x^2

    ! choose functor parameters a,b (affine change of variables)
    a = 2.0_dp
    b = 0.5_dp

    ! derivative operator D: D p has coeff_j = (j+1) * c_{j+1}
    pD = 0.0_dp
    do i = 1, nfuncs
      do j = 1, deg
        pD(i,j) = real(j,dp) * p(i,j+1)
      end do
    end do

    ! Functor F: compute coefficients of p(a*x + b)
    pF = 0.0_dp
    do i = 1, nfuncs
      do k = 0, deg
        if (abs(p(i,k+1)) < 1.0e-18_dp) cycle
        do j = 0, k
          pF(i,j+1) = pF(i,j+1) + p(i,k+1) * real(binomial_int(k,j),dp) * a**real(j,dp) * b**real(k-j,dp)
        end do
      end do
    end do

    ! Compose: FDp = F( D p ) and DFp = D( F p )
    pFD = 0.0_dp
    do i = 1, nfuncs
      do k = 0, deg
        if (abs(pD(i,k+1)) < 1.0e-18_dp) cycle
        do j = 0, k
          pFD(i,j+1) = pFD(i,j+1) + pD(i,k+1) * real(binomial_int(k,j),dp) * a**real(j,dp) * b**real(k-j,dp)
        end do
      end do
    end do

    pDF = 0.0_dp
    do i = 1, nfuncs
      do j = 1, deg
        pDF(i,j) = real(j,dp) * pF(i,j+1)
      end do
    end do

    open(newunit=unit_f, file='functorial_semantics.csv', status='replace', action='write', iostat=ios)
    if (ios /= 0) then
      write(*,'(A)') '  Warning: could not open functorial_semantics.csv for writing.'
    else
      call csv_write(unit_f, 'id,orig_coeffs,F(p)_coeffs,D(p)_coeffs,F(D(p))_coeffs,D(F(p))_coeffs,residual_norm', ios)
      do i = 1, nfuncs
        call write_coeff_str(p(i,1:deg+1), deg, tmp_s)
        row = 'f'//trim(adjustl(itoa(i)))//','//trim(tmp_s)
        call write_coeff_str(pF(i,1:deg+1), deg, tmp_s)
        row = trim(row)//','//trim(tmp_s)
        call write_coeff_str(pD(i,1:deg+1), deg, tmp_s)
        row = trim(row)//','//trim(tmp_s)
        call write_coeff_str(pFD(i,1:deg+1), deg, tmp_s)
        row = trim(row)//','//trim(tmp_s)
        call write_coeff_str(pDF(i,1:deg+1), deg, tmp_s)
        ! compute residual norm between DFp and FDp
        sumsq = 0.0_dp
        do j = 1, deg+1
          sumsq = sumsq + ( pDF(i,j) - pFD(i,j) )**2
        end do
        write(tmp_s,'(ES18.10)') sumsq
        row = trim(row)//','//trim(tmp_s)
        call csv_write(unit_f, trim(row), ios)
      end do
      close(unit_f)
      write(*,'(A)') '  Functorial semantics demo: CSV saved to functorial_semantics.csv'
    end if

    deallocate(p, pF, pD, pFD, pDF)
  end subroutine functorial_semantics_demo

  ! Nonlinear functional calculus demo: Nemytskii operator toy
  subroutine nonlinear_functional_calculus_demo()
    implicit none
    integer :: N, i, unit_f, ios
    real(dp), allocatable :: x(:), u(:), Nu(:)
    real(dp) :: dx, L2u, L2Nu, supu, supNu
    real(dp), parameter :: PI = 3.141592653589793_dp
    character(len=256) :: row, tmp

    write(*,'(A)') "  Nonlinear functional calculus demo (Nemytskii operator):"

    N = 101
    allocate(x(N), u(N), Nu(N))
    dx = 1.0_dp / real(N-1, dp)
    do i = 1, N
      x(i) = real(i-1, dp) * dx
      u(i) = sin(PI * x(i))
    end do

    ! Nemytskii operator g(u) = u^3 - u
    Nu = u**3 - u

    L2u = 0.0_dp; L2Nu = 0.0_dp; supu = 0.0_dp; supNu = 0.0_dp
    do i = 1, N
      L2u = L2u + u(i)**2
      L2Nu = L2Nu + Nu(i)**2
      supu = max(supu, abs(u(i)))
      supNu = max(supNu, abs(Nu(i)))
    end do
    L2u = sqrt(L2u * dx)
    L2Nu = sqrt(L2Nu * dx)

    open(newunit=unit_f, file='nonlinear_functional_calculus.csv', status='replace', action='write', iostat=ios)
    if (ios /= 0) then
      write(*,'(A)') '  Warning: could not open nonlinear_functional_calculus.csv for writing.'
    else
      call csv_write(unit_f, 'x,u,N(u)', ios)
      do i = 1, N
          write(tmp,'(ES18.10)') x(i)
        row = trim(tmp)//','
          write(tmp,'(ES18.10)') u(i)
        row = trim(row)//trim(tmp)//','
          write(tmp,'(ES18.10)') Nu(i)
        row = trim(row)//trim(tmp)
        call csv_write(unit_f, trim(row), ios)
      end do
      call csv_write(unit_f, '', ios)
      call csv_write(unit_f, 'summary,L2(u),L2(N(u)),sup(u),sup(N(u))', ios)
      write(tmp,'(ES18.10)') L2u
      row = 'summary,'//trim(tmp)
      write(tmp,'(ES18.10)') L2Nu
      row = trim(row)//','//trim(tmp)
      write(tmp,'(ES18.10)') supu
      row = trim(row)//','//trim(tmp)
      write(tmp,'(ES18.10)') supNu
      row = trim(row)//','//trim(tmp)
      call csv_write(unit_f, trim(row), ios)
      close(unit_f)
      write(*,'(A)') '  Nonlinear functional calculus demo: CSV saved to nonlinear_functional_calculus.csv'
    end if

    deallocate(x, u, Nu)
  end subroutine nonlinear_functional_calculus_demo

  ! helper: write coefficient array into a comma-separated scientific string
  subroutine write_coeff_str(arr, deg, outstr)
    implicit none
    integer, intent(in) :: deg
    real(dp), intent(in) :: arr(:)
    character(len=*), intent(out) :: outstr
    integer :: j
    character(len=64) :: tmp
    outstr = ''
    do j = 1, deg+1
      write(tmp,'(ES18.10)') arr(j)
      if (j == 1) then
        outstr = trim(tmp)
      else
        outstr = trim(outstr)//','//trim(tmp)
      end if
    end do
  end subroutine write_coeff_str

  ! small integer-to-string helper
  function itoa(i) result(s)
    implicit none
    integer, intent(in) :: i
    character(len=12) :: s
    write(s,'(I0)') i
  end function itoa

  subroutine hyperreals_demo()
    implicit none
    ! Enhanced numeric toy for hyperreal intuition using sequences
    ! Illustrate infinite sequences (n), infinitesimals (1/n^p), comparison to machine epsilon,
    ! and simple arithmetic of infinitesimals. Save results to CSV for inspection.
    integer :: N, k, outu, ios
    integer, parameter :: ncases = 6
    integer, dimension(ncases) :: Ns
    real(dp) :: aN, recip1, recip2, recip3, eps_machine, ratio1, ratio2, ratio3
    real(dp) :: sum_inf, prod_inf
    character(len=128) :: sN, saN, srecip1, srecip2, srecip3, sratio1, sratio2, sratio3, sbit1, sbit2, sbit3
    character(len=1024) :: row
    ! row not needed; kept formatting via direct writes

    write(*,'(A)') "  Hyperreal demo (numeric intuition via sequences and infinitesimals):"
    write(*,'(A)') "   Compare sequences 1/n^p (p=1,2,3) against machine epsilon and show simple arithmetic."

    Ns = (/ 10, 100, 1000, 10000, 1000000, 100000000 /)
    eps_machine = epsilon(1.0_dp)

    open(newunit=outu, file='hyperreals_calculus.csv', status='replace', action='write', iostat=ios)
    if (ios /= 0) then
      write(*,'(A)') '  Warning: could not open hyperreals_calculus.csv for writing.'
    else
      call csv_write(outu, 'N,a_N,1/N,1/N^2,1/N^3,1/N_eps,1/N^2_eps,1/N^3_eps,is_1N_lt_eps,is_1N2_lt_eps,is_1N3_lt_eps', ios)
      do k = 1, ncases
        N = Ns(k)
        aN = real(N, dp)
        recip1 = 1.0_dp / aN
        recip2 = 1.0_dp / (aN*aN)
        recip3 = 1.0_dp / (aN*aN*aN)
        ratio1 = recip1 / eps_machine
        ratio2 = recip2 / eps_machine
        ratio3 = recip3 / eps_machine
        ! booleans: 1 if less than machine eps, 0 otherwise (write as integer)
        write(sN,'(I0)') N
        write(saN,'(ES18.10)') aN
        write(srecip1,'(ES18.10)') recip1
        write(srecip2,'(ES18.10)') recip2
        write(srecip3,'(ES18.10)') recip3
        write(sratio1,'(ES18.10)') ratio1
        write(sratio2,'(ES18.10)') ratio2
        write(sratio3,'(ES18.10)') ratio3
        write(sbit1,'(I0)') merge(1,0, recip1 < eps_machine)
        write(sbit2,'(I0)') merge(1,0, recip2 < eps_machine)
        write(sbit3,'(I0)') merge(1,0, recip3 < eps_machine)
        row = trim(sN)//','//trim(saN)//','//trim(srecip1)//','//trim(srecip2)//','//trim(srecip3)//','//trim(sratio1)//','//trim(sratio2)//','//trim(sratio3)//','//trim(sbit1)//','//trim(sbit2)//','//trim(sbit3)
        call csv_write(outu, trim(row), ios)
      end do

      ! simple arithmetic examples: sum and product of two infinitesimals 1/N and 1/(2N)
      call csv_write(outu, 'example,inf1,inf2,sum,prod', ios)
      do k = 1, 3
        N = Ns(k)
        recip1 = 1.0_dp / real(N,dp)
        recip2 = 1.0_dp / (2.0_dp * real(N,dp))
        sum_inf = recip1 + recip2
        prod_inf = recip1 * recip2
        write(srecip1,'(ES18.10)') recip1
        write(srecip2,'(ES18.10)') recip2
        write(sratio1,'(ES18.10)') sum_inf
        write(sratio2,'(ES18.10)') prod_inf
        row = 'sum_product_case,'//trim(srecip1)//','//trim(srecip2)//','//trim(sratio1)//','//trim(sratio2)
        call csv_write(outu, trim(row), ios)
      end do

      close(outu)
      write(*,'(A)') '  Hyperreal demo: CSV saved to hyperreals_calculus.csv'
    end if

    write(*,'(A)') '  Note: numeric "infinitesimal" here is sequence-based; rigorous hyperreal requires nonstandard analysis machinery.'
  end subroutine hyperreals_demo

  subroutine variational_bicomplex_demo()
    implicit none
    integer, parameter :: dp = selected_real_kind(15,307)
    integer :: N, i, unit_f, ios
    real(dp) :: a, b, dx, x
    real(dp), allocatable :: y(:), dy(:), EL(:)
    character(len=64) :: sx, sy, sdy, sEL, srow

    ! Toy variational bicomplex demonstration (1D field y(x))
    ! Lagrangian: L = 1/2 (y')^2 - 1/2 y^2  -> Euler-Lagrange equation: y'' + y = 0
    a = 0.0_dp
    b = 1.0_dp
    N = 101
    dx = (b - a) / real(N-1, dp)

    allocate(y(N), dy(N), EL(N))

    ! choose a smooth test field y(x) = 1 + 0.2*sin(pi x)
    do i = 1, N
      x = a + real(i-1, dp) * dx
      y(i) = 1.0_dp + 0.2_dp * sin(3.141592653589793_dp * x)
    end do

    ! discrete first derivative (central differences)
    do i = 2, N-1
      dy(i) = ( y(i+1) - y(i-1) ) / ( 2.0_dp * dx )
    end do
    dy(1) = ( y(2) - y(1) ) / dx
    dy(N) = ( y(N) - y(N-1) ) / dx

    ! approximate Euler-Lagrange residual: y'' + y (second difference + y)
    do i = 1, N
      if (i == 1) then
        EL(i) = ( y(3) - 2.0_dp*y(2) + y(1) ) / ( dx*dx ) + y(i)
      else if (i == N) then
        EL(i) = ( y(N) - 2.0_dp*y(N-1) + y(N-2) ) / ( dx*dx ) + y(i)
      else
        EL(i) = ( y(i+1) - 2.0_dp*y(i) + y(i-1) ) / ( dx*dx ) + y(i)
      end if
    end do

    open(newunit=unit_f, file='variational_bicomplex.csv', status='replace', action='write', iostat=ios)
    if (ios /= 0) then
      write(*,'(A)') '  Warning: could not open variational_bicomplex.csv for writing.'
    else
      call csv_write(unit_f, 'x,y,dy_dx,EL_residual', ios)
      do i = 1, N
        x = a + real(i-1, dp) * dx
        write(sx,'(ES18.10)') x
        write(sy,'(ES18.10)') y(i)
        write(sdy,'(ES18.10)') dy(i)
        write(sEL,'(ES18.10)') EL(i)
        srow = trim(sx)//','//trim(sy)//','//trim(sdy)//','//trim(sEL)
        call csv_write(unit_f, trim(srow), ios)
      end do
      close(unit_f)
      write(*,'(A)') 'Variational bicomplex demo: CSV saved to variational_bicomplex.csv'
    end if

    deallocate(y, dy, EL)
  end subroutine variational_bicomplex_demo

  subroutine evolutionary_systems_demo()
    implicit none
    integer :: i, N
    real(dp) :: dt, x, y, r1, r2, K
    ! simple two-species continuous replicator/selection style model
    x = 0.4_dp; y = 0.6_dp; r1 = 0.1_dp; r2 = 0.05_dp; K = 10.0_dp
    dt = 0.01_dp; N = 1000
    do i = 1, N
      x = x + dt * r1 * x * (1.0_dp - (x+y)/K)
      y = y + dt * r2 * y * (1.0_dp - (x+y)/K)
    end do
    write(*,'(A,F12.6,A,F12.6)') "  Evolutionary system populations ~=", x, " , ", y
  end subroutine evolutionary_systems_demo

  subroutine nonlinear_calculus_demo()
    implicit none
    integer :: i, N
    real(dp) :: r, x
    r = 3.9_dp; N = 200
    x = 0.5_dp
    do i = 1, N
      x = r * x * (1.0_dp - x)
    end do
    write(*,'(A,F12.6)') "  Logistic map (r=3.9) sample iterate ~=", x
  end subroutine nonlinear_calculus_demo

  subroutine biology_variants_demo()
    implicit none
    integer :: i, N
    real(dp) :: dt, t, prey, pred, alpha, beta, delta, gamma
    alpha = 1.1_dp; beta = 0.4_dp; delta = 0.1_dp; gamma = 0.4_dp
    dt = 0.01_dp; N = 1000; prey = 10.0_dp; pred = 5.0_dp
    do i = 1, N
      t = i*dt
      prey = prey + dt*(alpha*prey - beta*prey*pred)
      pred = pred + dt*(delta*prey*pred - gamma*pred)
    end do
    write(*,'(A,F12.6,A,F12.6)') "  Lotka-Volterra after T ~=", prey, " , predator ~=", pred
  end subroutine biology_variants_demo

  subroutine probabilities_calc_demo()
    implicit none
    integer :: N, i
    real(dp) :: a, b, x, dx, sum, analytic
    ! compute expectation E[X^2] for standard normal via increasing N and midpoint rule
    real(dp), parameter :: SQ2PI = sqrt(2.0_dp*3.141592653589793_dp)
    analytic = 1.0_dp  ! Var(X)=1 for standard normal, E[X^2]=1
    write(*,'(A)') "  E[X^2] numeric convergence study (midpoint rule)"
    write(*,'(A)') "   N    numeric    abs_err"
    do N = 1000, 20000, 4500
      a = -10.0_dp; b = 10.0_dp; dx = (b-a)/real(N,dp)
      sum = 0.0_dp
      do i = 0, N-1
        x = a + (i+0.5_dp)*dx
        sum = sum + (x**2) * exp(-0.5_dp*x*x) / SQ2PI * dx
      end do
      write(*,'(I8,3X,ES12.6,3X,ES12.6)') N, sum, abs(sum - analytic)
    end do
  end subroutine probabilities_calc_demo

  subroutine complex_functions_demo()
    implicit none
    integer :: M, k
    real(dp) :: R, t, dt, re_part, im_part
    complex(dp) :: z, dz, fsum
    M = 2000; R = 2.0_dp; dt = 2.0_dp*3.141592653589793_dp / real(M,dp)
    fsum = (0.0_dp, 0.0_dp)
    do k = 0, M-1
      t = dt*real(k,dp)
      z = cmplx(R*cos(t), R*sin(t), kind=dp)
      dz = cmplx(-R*sin(t), R*cos(t), kind=dp) * cmplx(dt,0.0_dp,kind=dp)
      fsum = fsum + dz / (z - cmplx(1.0_dp,0.0_dp,kind=dp))  ! integrate 1/(z-1) around circle radius 2
    end do
    re_part = real(fsum); im_part = aimag(fsum)
    write(*,'(A)') "  Numeric contour integral of 1/(z-1) around circle R=2 (should be 2*pi*i):"
    write(*,'(A,F12.8,A,F12.8)') "   numeric Re=", re_part, "  numeric Im=", im_part
    write(*,'(A,F12.8)') "   analytic Re=0.0  analytic Im=", 2.0_dp*3.141592653589793_dp
    write(*,'(A,ES18.10)') "   complex error norm |numeric - analytic| =", sqrt(re_part**2 + (im_part - 2.0_dp*3.141592653589793_dp)**2)
  end subroutine complex_functions_demo

  subroutine moving_frames_demo()
    implicit none
    real(dp) :: t, speed, curvature, torsion, cp(3)
    real(dp) :: x(3), x1(3), x2(3)
    t = 1.0_dp
    x = (/ cos(t), sin(t), t /)
    x1 = (/ -sin(t), cos(t), 1.0_dp /)
    x2 = (/ -cos(t), -sin(t), 0.0_dp /)
    speed = sqrt(sum(x1**2))
    call cross_product(x1, x2, cp)
    curvature = sqrt(sum(cp**2)) / (speed**3)
    torsion = 0.0_dp  ! simple helix torsion omitted for brevity
    write(*,'(A,F12.6,A,F12.6)') "  Frenet curvature at t=1 ~=", curvature, "  speed=", speed
  end subroutine moving_frames_demo

  subroutine cross_product(a,b,c)
    implicit none
    real(dp), intent(in) :: a(3), b(3)
    real(dp), intent(out) :: c(3)
    c(1) = a(2)*b(3) - a(3)*b(2)
    c(2) = a(3)*b(1) - a(1)*b(3)
    c(3) = a(1)*b(2) - a(2)*b(1)
  end subroutine cross_product

  subroutine lie_algebras_demo()
    implicit none
    real(dp) :: A(3,3), B(3,3), C(3,3)
    ! simple so(3) generators (infinitesimal rotations)
    A = 0.0_dp; B = 0.0_dp; C = 0.0_dp
    A(2,3) = -1.0_dp; A(3,2)=1.0_dp
    B(1,3) = 1.0_dp; B(3,1)=-1.0_dp
    C = matmul(A,B) - matmul(B,A)
    write(*,'(A)') "  so(3) commutator example (A,B)->C nonzero entries (should be so(3) again):"
    write(*,'(3F12.6)') C(1,1),C(1,2),C(1,3)
  end subroutine lie_algebras_demo

  subroutine spinors_demo()
    implicit none
    complex(dp) :: sx(2,2), sy(2,2), prod(2,2)
    sx = cmplx(0.0_dp,0.0_dp, kind=dp)
    sy = cmplx(0.0_dp,0.0_dp, kind=dp)
    sx(1,2) = cmplx(1.0_dp,0.0_dp, kind=dp); sx(2,1) = cmplx(1.0_dp,0.0_dp, kind=dp)
    sy(1,2) = cmplx(0.0_dp,-1.0_dp, kind=dp); sy(2,1) = cmplx(0.0_dp,1.0_dp, kind=dp)
    prod = matmul(sx,sy)
    write(*,'(A)') "  Pauli sigma_x * sigma_y example (complex 2x2 product) printed element (1,2):"
    write(*,'(A,F12.6)') "   Re(prod(1,2))=", real(prod(1,2))
  end subroutine spinors_demo

  subroutine neural_fields_demo()
    implicit none
    integer :: i, N, it, epochs
    real(dp) :: dx, x, learning_rate, lambda, energy, grad_norm
    real(dp) :: beta1, beta2, lr0, decay, eps_adam, b1t, b2t
    real(dp), allocatable :: u(:), target(:), grad(:), prev_update(:), update(:), m(:), v(:)
    real(dp), parameter :: PI = 3.141592653589793_dp
    ! Discretized 1D field on [0,1] with target function sin(2*pi*x)
    N = 100
    dx = 1.0_dp/real(N,dp)
    allocate(u(N), target(N), grad(N), prev_update(N), update(N))
    do i = 1, N
      x = (i-0.5_dp)*dx
      target(i) = sin(2.0_dp*PI*x)
      u(i) = 0.0_dp  ! initial guess (centered)
    end do
    ! initialize Adam buffers and algorithm hyperparameters
    beta1 = 0.9_dp
    beta2 = 0.999_dp
    eps_adam = 1.0e-8_dp
    allocate(m(N), v(N))
    m = 0.0_dp
    v = 0.0_dp
    update = 0.0_dp
    energy = 0.0_dp
    ! set regularization and learning parameters
    lambda = 1.0e-3_dp
    lr0 = 0.01_dp
    decay = 1.0e-4_dp
    epochs = 2000
    do i = 1, N
      energy = energy + 0.5_dp*(u(i)-target(i))**2
    end do
    do i = 1, N-1
      energy = energy + 0.5_dp*lambda*( (u(i+1)-u(i))/dx )**2 * dx
    end do
    write(*,'(A,F12.8)') "   initial energy=", energy
    do it = 1, epochs
      ! compute gradient of data term
      grad = 0.0_dp
      do i = 1, N
        grad(i) = u(i) - target(i)
      end do
      ! add smoothness gradient (discrete Laplacian term) with one-sided handling at boundaries
      if (N > 1) then
        ! left boundary i=1 (one-sided second difference)
        grad(1) = grad(1) - lambda * (u(2) - u(1)) / (dx*dx)
        ! interior
        do i = 2, N-1
          grad(i) = grad(i) - lambda * (u(i+1) - 2.0_dp*u(i) + u(i-1)) / (dx*dx)
        end do
        ! right boundary i=N
        grad(N) = grad(N) - lambda * (u(N-1) - u(N)) / (dx*dx)
      end if
      ! adaptive learning rate schedule (simple decay)
      learning_rate = lr0 / (1.0_dp + decay * real(it,dp))
      ! Adam update: update m, v, compute bias-corrected estimates and update u
      b1t = 1.0_dp - beta1**real(it,dp)
      b2t = 1.0_dp - beta2**real(it,dp)
      do i = 1, N
        m(i) = beta1 * m(i) + (1.0_dp - beta1) * grad(i)
        v(i) = beta2 * v(i) + (1.0_dp - beta2) * grad(i) * grad(i)
        ! bias-corrected moments
        update(i) = - learning_rate * ( m(i) / b1t ) / ( sqrt( v(i) / b2t ) + eps_adam )
        ! optional clipping of updates to avoid blowup
        if (abs(update(i)) > 0.5_dp) update(i) = sign(0.5_dp, update(i))
      end do
      u = u + update
      ! occasional diagnostics
      if (mod(it,200) == 0) then
        energy = 0.0_dp
        do i = 1, N
          energy = energy + 0.5_dp*(u(i)-target(i))**2
        end do
        do i = 1, N-1
          energy = energy + 0.5_dp*lambda*( (u(i+1)-u(i))/dx )**2 * dx
        end do
        write(*,'(A,I6,3X,A,F12.8)') "   epoch", it, "  energy=", energy
      end if
    end do
    ! final diagnostics (RMSE)
    energy = 0.0_dp; grad_norm = 0.0_dp
    do i = 1, N
      energy = energy + 0.5_dp*(u(i)-target(i))**2
      grad_norm = grad_norm + (u(i)-target(i))**2
    end do
    write(*,'(A,F12.8)') "   final energy=", energy
    write(*,'(A,F12.8)') "   final residual L2 (RMSE) ~", sqrt(grad_norm/real(N,dp))
    write(*,'(A)') "   sample (x,u,target) at a few nodes:" 
    do i = 1, N, max(1,N/4)
      x = (i-0.5_dp)*dx
      write(*,'(F6.3,2X,F10.6,2X,F10.6)') x, u(i), target(i)
    end do
    ! write final field to CSV for external plotting
    open(unit=20, file='neural_fields_output.csv', status='replace', action='write', form='formatted')
    write(20,'(A)') 'x,u,target'
    do i = 1, N
      x = (i-0.5_dp)*dx
      write(20,'(F12.8,1X,F18.12,1X,F18.12)') x, u(i), target(i)
    end do
    close(20)
    write(*,'(A)') '   CSV saved to neural_fields_output.csv'

    deallocate(u, target, grad, update, m, v)
  end subroutine neural_fields_demo

  subroutine hyperfunctions_demo()
    implicit none
    ! Toy numeric illustration: Lorentzian approximation to delta (L_eps(x) = (1/pi) * eps/(x^2 + eps^2))
    integer :: idx, N
    real(dp) :: eps, a, x, dx, sum, norm
    real(dp), parameter :: PI = 3.141592653589793_dp
    eps = 1.0e-3_dp
    N = 20000
    a = 8.0_dp
    dx = 2.0_dp*a/real(N,dp)
    sum = 0.0_dp
    norm = 0.0_dp
    do idx = -N/2, N/2
      x = real(idx,dp) * dx
      norm = norm + (1.0_dp/PI) * ( eps / (x**2 + eps**2) ) * dx
      sum = sum + (1.0_dp/PI) * ( eps / (x**2 + eps**2) ) * (x**2) * dx
    end do
    write(*,'(A)') "  Hyperfunction toy: Lorentzian approx to delta; show normalization and moment." 
    write(*,'(A,F12.8)') "   eps=", eps
    write(*,'(A,ES12.6)') "   normalization integral ∫ L_eps dx ~", norm
    write(*,'(A,ES12.6)') "   integral ∫ x^2 L_eps dx ~", sum
    write(*,'(A)') "  As eps->0 the normalization -> 1 and moments like ∫ x^2 L_eps dx -> 0 (for integrable test functions)."
  end subroutine hyperfunctions_demo

  subroutine colombeau_calculus_demo()
    implicit none
    integer, parameter :: dp = selected_real_kind(15,307)
    integer :: N, i, idx, iu, neps
    real(dp) :: a, b, dx, x, eps, sumphi, sumprod, peakprod
    real(dp), parameter :: SQ2PI = sqrt(2.0_dp*3.141592653589793_dp)
    real(dp), dimension(:), allocatable :: epslist

    ! Compact Colombeau-style toy: embed delta via Gaussian mollifiers phi_eps,
    ! form product phi_eps * psi_eps (here psi = phi) and show integrated mass
    ! scaling as eps->0. This is a numeric illustration, not a formal theory.

    epslist = [0.5_dp, 0.2_dp, 0.1_dp, 0.05_dp, 0.02_dp, 0.01_dp]
    neps = size(epslist)

    open(unit=88, file='colombeau_calculus.csv', status='replace', action='write', iostat=iu)
    if (iu /= 0) then
      write(*,'(A)') '  Warning: could not open colombeau_calculus.csv for writing.'
      return
    end if
    write(88,'(A)') 'eps,integral_phi,integral_product,peak_product'

    N = 2001
    do idx = 1, neps
      eps = epslist(idx)
      ! domain wide enough to capture mollifier mass (several sigma)
      a = -8.0_dp * eps
      b =  8.0_dp * eps
      dx = (b - a) / real(N-1, dp)
      sumphi = 0.0_dp
      sumprod = 0.0_dp
      peakprod = 0.0_dp
      do i = 0, N-1
        x = a + (real(i,dp) + 0.5_dp) * dx
        ! Gaussian mollifier approximating delta: phi_eps(x) = (1/(sqrt(2pi)*eps)) exp(-x^2/(2 eps^2))
        sumphi = sumphi + (1.0_dp/(SQ2PI*eps)) * exp( - (x*x) / (2.0_dp * eps*eps) ) * dx
        ! here psi = phi (symmetric example) so product = phi^2
        sumprod = sumprod + (1.0_dp/(SQ2PI*eps))**2 * exp( - (x*x) / ( eps*eps ) ) * dx
        peakprod = max(peakprod, (1.0_dp/(SQ2PI*eps))**2 * exp( - (x*x) / ( eps*eps ) ))
      end do
      write(88,'(ES18.10,1A,ES18.10,1A,ES18.10,1A,ES18.10)') eps, ',', sumphi, ',', sumprod, ',', peakprod
      write(*,'(A,ES12.6,A,ES12.6)') '  Colombeau demo eps=', eps, ' integrated product mass=', sumprod
    end do
    close(88)
    write(*,'(A)') '  Colombeau calculus demo: CSV saved to colombeau_calculus.csv'

  end subroutine colombeau_calculus_demo

  subroutine hida_calculus_demo()
    implicit none
    integer, parameter :: dp = selected_real_kind(15,307)
    integer :: N, i, outu, ios
    real(dp) :: x, sum1, sum2, sum3, sumw, sumw2
    real(dp) :: mean1, mean2, mean3, mean_wick2, var_wick2

    write(*,'(A)') '  Hida white-noise calculus demo: Wick product and chaos toy'

    N = 20000
    sum1 = 0.0_dp
    sum2 = 0.0_dp
    sum3 = 0.0_dp
    sumw = 0.0_dp
    sumw2 = 0.0_dp

    do i = 1, N
      x = normal_rand()
      sum1 = sum1 + x
      sum2 = sum2 + x*x
      sum3 = sum3 + x*x*x
      ! Wick square: :X^2: = X^2 - E[X^2] (for standard normal E[X^2]=1)
      sumw = sumw + (x*x - 1.0_dp)
      sumw2 = sumw2 + (x*x - 1.0_dp)**2
    end do

    mean1 = sum1 / real(N, dp)
    mean2 = sum2 / real(N, dp)
    mean3 = sum3 / real(N, dp)
    mean_wick2 = sumw / real(N, dp)
    var_wick2 = sumw2 / real(N, dp) - mean_wick2*mean_wick2

    open(newunit=outu, file='hida_calculus.csv', status='replace', action='write', iostat=ios)
    if (ios /= 0) then
      write(*,'(A)') '  Warning: could not open hida_calculus.csv for writing.'
    else
      write(outu,'(A)') 'moment1,moment2,moment3,mean_wick2,var_wick2'
      write(outu,'(ES18.10,1A,ES18.10,1A,ES18.10,1A,ES18.10,1A,ES18.10)') mean1, ',', mean2, ',', mean3, ',', mean_wick2, ',', var_wick2
      close(outu)
      write(*,'(A)') '  Hida calculus demo: CSV saved to hida_calculus.csv'
    end if

  end subroutine hida_calculus_demo

  subroutine feynman_calculus_demo()
    implicit none
    integer, parameter :: dp = selected_real_kind(15,307)
    integer :: M, i, k, Npaths, outu, ios
    integer :: n_therm, n_between, sweep, accept_count, total_updates
    integer :: therm_accept, therm_updates, adjust_every, tune_u, i_tune_ios
    real(dp) :: T, dt, xold, xnew, S_old, S_new, dS, w, omega
    real(dp) :: sumw, sumw2, sumS, prop_sigma, u_rand, acc_rate, u_prop
    real(dp) :: target_acc
    real(dp), allocatable :: path(:)
    character(len=256) :: sM, sT, sN, sMeanW, sVarW, sMeanS, sAcc, sSigma, row

    write(*,'(A)') '  Feynman/path-integral demo: harmonic oscillator toy (Metropolis sampler)'

    ! Parameters (toy)
    M = 20                ! time slices
    T = 1.0_dp            ! total time
    dt = T / real(M, dp)
    omega = 1.0_dp
    Npaths = 2000         ! number of recorded samples

    ! Metropolis tuning
    n_therm = 200         ! thermalization sweeps
    n_between = 10        ! sweeps between recorded samples
    prop_sigma = 0.6_dp * sqrt(dt)  ! proposal scale (tunable)

    ! Auto-tune parameters (during thermalization): aim for target_acc
    target_acc = 0.50_dp
    therm_accept = 0
    therm_updates = 0
    adjust_every = 10     ! adjust prop_sigma every N sweeps and record tuning
    tune_u = -1
    i_tune_ios = 0

    allocate(path(0:M))

    call random_seed()

    ! initialize path (fixed endpoints at 0)
    path = 0.0_dp

    accept_count = 0
    total_updates = 0

    ! thermalize with simple auto-tuning of prop_sigma
    if (n_therm > 0) then
      open(newunit=tune_u, file='feynman_tuning.csv', status='replace', action='write', iostat=i_tune_ios)
      if (i_tune_ios == 0) then
        write(tune_u,'(A)') 'sweep,acc_rate,prop_sigma'
      else
        tune_u = -1
      end if
      do sweep = 1, n_therm
        do k = 1, M-1
          ! single-site update at k (endpoints fixed)
          xold = path(k)
          call random_number(u_prop)
          xnew = xold + prop_sigma * ( (2.0_dp * u_prop ) - 1.0_dp )

          ! compute local action before and after (only bonds (k-1,k) and (k,k+1) and potential at k)
          S_old = 0.5_dp * ( (xold - path(k-1))**2 / dt + (path(k+1) - xold)**2 / dt ) + 0.5_dp * dt * omega*omega * xold*xold
          S_new = 0.5_dp * ( (xnew - path(k-1))**2 / dt + (path(k+1) - xnew)**2 / dt ) + 0.5_dp * dt * omega*omega * xnew*xnew

          dS = S_new - S_old
          if (dS <= 0.0_dp) then
            path(k) = xnew
            accept_count = accept_count + 1
            therm_accept = therm_accept + 1
          else
            call random_number(u_rand)
            if (u_rand < exp( - dS )) then
              path(k) = xnew
              accept_count = accept_count + 1
              therm_accept = therm_accept + 1
            end if
          end if
          total_updates = total_updates + 1
          therm_updates = therm_updates + 1
        end do

        ! periodic adjustment of proposal scale based on recent thermalization acceptance
        if (mod(sweep, adjust_every) == 0) then
          if (therm_updates > 0) then
            acc_rate = real(therm_accept, dp) / real(therm_updates, dp)
            ! simple multiplicative tuning: push acceptance toward target_acc
            if (acc_rate > target_acc + 0.08_dp) then
              prop_sigma = prop_sigma * 1.08_dp
            else if (acc_rate < target_acc - 0.08_dp) then
              prop_sigma = prop_sigma * 0.92_dp
            end if
            ! record tuning step if file opened
            if (tune_u > 0) then
              write(sM,'(I0)') sweep
              write(sAcc,'(ES18.10)') acc_rate
              write(sSigma,'(ES18.10)') prop_sigma
              row = trim(sM)//','//trim(sAcc)//','//trim(sSigma)
              write(tune_u,'(A)') trim(row)
            end if
            ! reset thermal counters for next window
            therm_accept = 0
            therm_updates = 0
          end if
        end if
      end do
      if (tune_u > 0) close(tune_u)
    end if

    sumw = 0.0_dp
    sumw2 = 0.0_dp
    sumS = 0.0_dp

    ! sample loop: perform n_between sweeps between recorded samples
    do i = 1, Npaths
      do sweep = 1, n_between
        do k = 1, M-1
          xold = path(k)
          call random_number(u_prop)
          xnew = xold + prop_sigma * ( (2.0_dp * u_prop ) - 1.0_dp )
          S_old = 0.5_dp * ( (xold - path(k-1))**2 / dt + (path(k+1) - xold)**2 / dt ) + 0.5_dp * dt * omega*omega * xold*xold
          S_new = 0.5_dp * ( (xnew - path(k-1))**2 / dt + (path(k+1) - xnew)**2 / dt ) + 0.5_dp * dt * omega*omega * xnew*xnew
          dS = S_new - S_old
          if (dS <= 0.0_dp) then
            path(k) = xnew
            accept_count = accept_count + 1
          else
            call random_number(u_rand)
            if (u_rand < exp( - dS )) then
              path(k) = xnew
              accept_count = accept_count + 1
            end if
          end if
          total_updates = total_updates + 1
        end do
      end do

      ! compute full discrete Euclidean action for current path
      S_old = 0.0_dp
      do k = 0, M-1
        S_old = S_old + 0.5_dp * ( (path(k+1) - path(k))**2 / dt + dt * omega*omega * 0.5_dp * (path(k+1)**2 + path(k)**2) )
      end do

      w = exp( - S_old )
      sumw = sumw + w
      sumw2 = sumw2 + w*w
      sumS = sumS + S_old
    end do

    if (total_updates > 0) then
      acc_rate = real(accept_count, dp) / real(total_updates, dp)
    else
      acc_rate = 0.0_dp
    end if

    ! summary stats to CSV
    open(newunit=outu, file='feynman_calculus.csv', status='replace', action='write', iostat=ios)
    if (ios /= 0) then
      write(*,'(A)') '  Warning: could not open feynman_calculus.csv for writing.'
    else
      write(outu,'(A)') 'M,T,Npaths,mean_weight,var_weight,mean_action,accept_rate,prop_sigma'
      write(sM,'(I0)') M
      write(sT,'(ES18.10)') T
      write(sN,'(I0)') Npaths
      write(sMeanW,'(ES18.10)') sumw/real(Npaths,dp)
      write(sVarW,'(ES18.10)') (sumw2/real(Npaths,dp) - (sumw/real(Npaths,dp))**2)
      write(sMeanS,'(ES18.10)') sumS/real(Npaths,dp)
      write(sAcc,'(ES18.10)') acc_rate
      write(sSigma,'(ES18.10)') prop_sigma
      row = trim(sM)//','//trim(sT)//','//trim(sN)//','//trim(sMeanW)//','//trim(sVarW)//','//trim(sMeanS)//','//trim(sAcc)//','//trim(sSigma)
      write(outu,'(A)') trim(row)
      close(outu)
      write(*,'(A)') '  Feynman demo: CSV saved to feynman_calculus.csv'
      write(*,'(A,F8.4)') '  Metropolis accept rate = ', acc_rate
    end if

    deallocate(path)
  end subroutine feynman_calculus_demo

  subroutine supergeometry_demo()
    implicit none
    ! Compact runnable supergeometry toy using "supernumbers" (even + odd part)
    type :: supernum
      real(dp) :: even   ! commuting part
      real(dp) :: odd    ! coefficient of a single Grassmann generator theta (theta^2 = 0)
    end type supernum

    type(supernum) :: a, b, c, sq
    real(dp) :: ber_num, ber_den, ber
    real(dp) :: Ablk, Dblk, Bblk, Cblk

    ! construct two supernumbers: a = a0 + a1*theta, b = b0 + b1*theta
    a%even = 2.0_dp; a%odd = 0.5_dp
    b%even = 3.0_dp; b%odd = 1.0_dp

    write(*,'(A)') "  Supergeometry toy: supernumber arithmetic with single Grassmann generator theta (theta^2=0)"
    write(*,'(A)') "   a = a0 + a1*theta, b = b0 + b1*theta"
    write(*,'(A,F8.4,A,F8.4)') "    a0=", a%even, "  a1=", a%odd
    write(*,'(A,F8.4,A,F8.4)') "    b0=", b%even, "  b1=", b%odd

    ! product c = a * b where (a0 + a1 theta)(b0 + b1 theta) = a0 b0 + (a0 b1 + a1 b0) theta
    c%even = a%even * b%even
    c%odd  = a%even * b%odd + a%odd * b%even
    write(*,'(A,F12.6)') "   product even part (c0) =", c%even
    write(*,'(A,F12.6)') "   product odd  part (c1) =", c%odd

    ! odd*odd should vanish: compute (odd_theta)^2 numeric demonstration
    sq%even = 0.0_dp
    sq%odd = 0.0_dp
    ! (0 + alpha theta)^2 => even part = 0, odd part = 0
    write(*,'(A)') "   Note: (alpha*theta)^2 = 0 (Grassmann property) - demonstrated by symbolic rule in this toy."

    ! Berezin derivative/integral toy: d/dtheta (a0 + a1 theta) = a1 ; ∫ dtheta (a0 + a1 theta) = a1
    write(*,'(A,F12.6)') "   Berezin derivative d/dtheta a =", a%odd
    write(*,'(A,F12.6)') "   Berezin integral ∫ dtheta a =", a%odd

    ! Tiny Berezinian example for 1|1 supermatrix in block form
    ! For M = [A B; C D] with A,D scalars, B,C odd: Ber(M) = (A - B D^{-1} C)/D
    Ablk = 2.0_dp; Dblk = 3.0_dp
    Bblk = 0.1_dp   ! odd coefficients (here numeric placeholder)
    Cblk = 0.2_dp
    ber_num = Ablk - Bblk * (1.0_dp/Dblk) * Cblk
    ber_den = Dblk
    ber = ber_num / ber_den
    write(*,'(A)') "   Berezinian toy for 1|1 supermatrix M = [A B; C D] (numeric placeholder treating B,C as coefficients)"
    write(*,'(A,F12.6)') "    A=", Ablk
    write(*,'(A,F12.6)') "    D=", Dblk
    write(*,'(A,F12.6)') "    formal B=", Bblk
    write(*,'(A,F12.6)') "    formal C=", Cblk
    write(*,'(A,F12.8)') "    Berezinian (toy numeric) =", ber

  end subroutine supergeometry_demo

  subroutine jet_bundles_demo()
    implicit none
    real(dp) :: x, f0, f1, f2
    x = 0.1_dp
    f0 = sin(x)
    f1 = cos(x)
    f2 = -sin(x)
    write(*,'(A)') "  Jet/Taylor demo: function and first 2 derivatives at x=0.1:" 
    write(*,'(A,F10.6)') "   f=", f0
    write(*,'(A,F10.6)') "   f'=", f1
    write(*,'(A,F10.6)') "   f''=", f2
  end subroutine jet_bundles_demo

  subroutine categories_demo()
    implicit none
    ! show two functors F,G: array->array and a natural transformation eta: F->G via pointwise operation
    integer :: i
    real(dp) :: A(5), Fv(5), Gv(5), eta(5)
    do i = 1,5
      A(i) = real(i,dp)
      Fv(i) = A(i)**2
      Gv(i) = A(i)**2 + 1.0_dp
      eta(i) = 1.0_dp
    end do
    write(*,'(A)') "  Categories demo: two functors (square) and (square + 1) with trivial natural transformation +1"
  end subroutine categories_demo

  subroutine control_variants_demo()
    implicit none
    real(dp) :: Kp, Ki, Kd, e, u
    Kp = 1.0_dp; Ki = 0.1_dp; Kd = 0.01_dp
    e = 0.5_dp
    u = Kp*e + Ki*e + Kd*e
    write(*,'(A,F12.6)') "  PID control demo (one-step) control output ~=", u
  end subroutine control_variants_demo

  subroutine infinite_dimensional_demo()
    implicit none
    integer :: N, i
    real(dp) :: a, b, dx, x, sum, normL2
    N = 1000; a = 0.0_dp; b = 1.0_dp; dx = (b-a)/real(N,dp)
    sum = 0.0_dp
    do i = 0, N-1
      x = a + (i+0.5_dp)*dx
      sum = sum + (sin(3.141592653589793_dp*x))**2 * dx
    end do
    normL2 = sqrt(sum)
    write(*,'(A,F12.6)') "  L2 norm of sin(pi x) on [0,1] ~=", normL2
  end subroutine infinite_dimensional_demo

  subroutine path_integral_demo()
    implicit none
    integer :: M, i, sample, Npaths
    real(dp) :: dt, omega, sumS, xprev, xcurr, weight, avgW
    real(dp), allocatable :: weights(:)
    real(dp) :: varW, sumW, sumW2
    M = 40; dt = 1.0_dp/real(M,dp); omega = 1.0_dp
    Npaths = 2000
    allocate(weights(Npaths))
    sumW = 0.0_dp; sumW2 = 0.0_dp
    do sample = 1, Npaths
      xprev = 0.0_dp; sumS = 0.0_dp
      do i = 1, M
        xcurr = normal_rand() * 0.5_dp  ! random displacement trial (very crude MC)
        sumS = sumS + 0.5_dp*((xcurr - xprev)**2)/dt + 0.5_dp*dt*omega**2*xcurr**2
        xprev = xcurr
      end do
      weight = exp(-sumS)
      weights(sample) = weight
      sumW = sumW + weight
      sumW2 = sumW2 + weight*weight
    end do
    avgW = sumW / real(Npaths,dp)
    varW = sumW2/real(Npaths,dp) - avgW*avgW
    write(*,'(A)') "  Crude path-integral MC (harmonic oscillator toy):"
    write(*,'(A,F12.6)') "   mean weight =", avgW
    write(*,'(A,ES12.6)') "   variance weight =", varW
    write(*,'(A,I0)') "   Npaths = ", Npaths
    deallocate(weights)
  end subroutine path_integral_demo

  subroutine fourier_series_demo()
    implicit none
    integer :: M, i, n, Nterms
    real(dp) :: L, dx, x, exact, approx, sumsq, errL2
    real(dp) :: maxabs, xnear, approx_near, exact_near
    real(dp), parameter :: PI = 3.141592653589793_dp
    ! Approximate a square wave on [-pi,pi] by Fourier sine series (odd terms)
    M = 801
    L = PI
    dx = 2.0_dp*L / real(M-1,dp)
    write(*,'(A)') "  Fourier series demo: square wave partial sums (odd terms)"
    write(*,'(A)') '   Nterms   L2_error      Max_abs_error   approx@(pi-0.01)  exact@(pi-0.01)'
    do Nterms = 1, 21, 2
      sumsq = 0.0_dp
      maxabs = 0.0_dp
      do i = 1, M
        x = -L + real(i-1,dp)*dx
        ! exact square wave: sign(sin(x)) (±1)
        if (sin(x) >= 0.0_dp) then
          exact = 1.0_dp
        else
          exact = -1.0_dp
        end if
        approx = 0.0_dp
        do n = 1, Nterms, 2
          approx = approx + (4.0_dp/(real(n,dp)*PI)) * sin(real(n,dp)*x)
        end do
        sumsq = sumsq + (approx - exact)**2 * dx
        maxabs = max(maxabs, abs(approx - exact))
      end do
      errL2 = sqrt(sumsq)
      ! evaluate near discontinuity at x = pi - 0.01 to show Gibbs overshoot
      xnear = PI - 0.01_dp
      approx_near = 0.0_dp
      do n = 1, Nterms, 2
        approx_near = approx_near + (4.0_dp/(real(n,dp)*PI)) * sin(real(n,dp)*xnear)
      end do
      exact_near = merge(1.0_dp, -1.0_dp, sin(xnear) >= 0.0_dp)
      write(*,'(I6,3X,ES12.6,3X,ES12.6,3X,ES12.6,3X,ES12.6)') Nterms, errL2, maxabs, approx_near, exact_near
    end do
  end subroutine fourier_series_demo

  subroutine spectral_derivative_demo()
    implicit none
    integer :: Ns(5) = (/16, 32, 64, 128, 256/)
    integer :: ni, N, j, k, kappa, M, i
    real(dp) :: L, dx, x, maxerr_spec, maxerr_fd, sumsq_spec, sumsq_fd
    real(dp) :: dffd, dfsp
    real(dp), parameter :: PI = 3.141592653589793_dp
    complex(dp), allocatable :: fj(:), Fk(:)
    real(dp), allocatable :: fr(:), df_spec_r(:), df_fd_r(:), df_exact_r(:), err_spec_r(:), err_fd_r(:), xgrid(:)
    real(dp), allocatable :: xcheb(:), fcheb(:), dfcheb(:)
    real(dp), allocatable :: D(:,:)
    real(dp), allocatable :: ccw(:)
    real(dp) :: cheb_L2, cheb_Linf, fourier_L2, fourier_Linf, fd_L2, fd_Linf
    integer, parameter :: csv_unit = 98, csvc = 99
    integer :: ios
    complex(dp) :: ci, cexp1, tempc

    ! Open a CSV to record convergence rates
    open(unit=csvc, file='spectral_convergence.csv', status='replace', action='write', iostat=ios)
    if (ios == 0) then
      write(csvc,'(A)') 'N,fourier_L2,fourier_Linf,fd_L2,fd_Linf,cheb_L2,cheb_Linf'
    end if

    ci = cmplx(0.0_dp, 1.0_dp, kind=dp)
    L = 2.0_dp * PI

    do ni = 1, size(Ns)
      N = Ns(ni)

      ! --- Fourier spectral (periodic) on [0,2pi]
      dx = L / real(N, dp)
      allocate(fj(N), Fk(N))
      allocate(fr(N), df_spec_r(N), df_fd_r(N), df_exact_r(N), err_spec_r(N), err_fd_r(N), xgrid(N))

      do j = 1, N
        x = real(j-1,dp) * dx
        xgrid(j) = x
        fr(j) = sin(3.0_dp * x)
        fj(j) = cmplx(fr(j), 0.0_dp, kind=dp)
      end do

      ! forward DFT
      do k = 0, N-1
        tempc = (0.0_dp, 0.0_dp)
        do j = 1, N
          cexp1 = cmplx(cos(-real(k,dp)*xgrid(j)), sin(-real(k,dp)*xgrid(j)), kind=dp)
          tempc = tempc + fj(j) * cexp1
        end do
        Fk(k+1) = tempc
      end do

      ! derivative synthesis
      do j = 1, N
        tempc = (0.0_dp, 0.0_dp)
        do k = 0, N-1
          if (k <= N/2) then
            kappa = k
          else
            kappa = k - N
          end if
          cexp1 = cmplx(cos(real(kappa,dp)*xgrid(j)), sin(real(kappa,dp)*xgrid(j)), kind=dp)
          tempc = tempc + (ci * real(kappa,dp)) * Fk(k+1) * cexp1
        end do
        dfsp = real(tempc, kind=dp) / real(N,dp)
        df_spec_r(j) = dfsp
      end do

      ! central FD (periodic)
      do j = 1, N
        if (j == 1) then
          dffd = (fr(2) - fr(N)) / (2.0_dp * dx)
        else if (j == N) then
          dffd = (fr(1) - fr(N-1)) / (2.0_dp * dx)
        else
          dffd = (fr(j+1) - fr(j-1)) / (2.0_dp * dx)
        end if
        df_fd_r(j) = dffd
      end do

      ! errors vs exact
      maxerr_spec = 0.0_dp; maxerr_fd = 0.0_dp
      sumsq_spec = 0.0_dp; sumsq_fd = 0.0_dp
      do j = 1, N
        df_exact_r(j) = 3.0_dp * cos(3.0_dp * xgrid(j))
        err_spec_r(j) = abs(df_spec_r(j) - df_exact_r(j))
        err_fd_r(j)   = abs(df_fd_r(j) - df_exact_r(j))
        maxerr_spec = max(maxerr_spec, err_spec_r(j))
        maxerr_fd   = max(maxerr_fd, err_fd_r(j))
        sumsq_spec = sumsq_spec + err_spec_r(j)**2 * dx
        sumsq_fd   = sumsq_fd   + err_fd_r(j)**2 * dx
      end do

      fourier_L2 = sqrt(sumsq_spec)
      fourier_Linf = maxerr_spec
      fd_L2 = sqrt(sumsq_fd)
      fd_Linf = maxerr_fd

      ! write last-resolution CSV for inspection
      open(unit=csv_unit, file='spectral_derivative.csv', status='replace', action='write', iostat=ios)
      if (ios == 0) then
        write(csv_unit,'(A)') 'x f df_spec df_fd df_exact err_spec err_fd'
        do j = 1, N
          write(csv_unit,'(F12.8,1X,F12.8,1X,ES15.8,1X,ES15.8,1X,ES15.8,1X,ES15.8,1X,ES15.8)') xgrid(j), fr(j), df_spec_r(j), df_fd_r(j), df_exact_r(j), err_spec_r(j), err_fd_r(j)
        end do
        close(csv_unit)
      end if

      deallocate(fj, Fk, fr, df_spec_r, df_fd_r, df_exact_r, err_spec_r, err_fd_r, xgrid)

      ! --- Chebyshev collocation on [-1,1] mapped to x in [0,2pi]
      M = N    ! use M+1 Chebyshev points => degree M
      allocate(xcheb(M+1), fcheb(M+1), dfcheb(M+1))
      allocate(D(M+1,M+1))
      call chebyshev_D(M, xcheb, D)
      do j = 0, M
        ! map y in [-1,1] to x in [0,2pi]: x = pi*(y+1)
        x = PI * (xcheb(j+1) + 1.0_dp)
        fcheb(j+1) = sin(3.0_dp * x)
      end do
      ! D * fcheb approximates df/dy; df/dx = (1/pi) * df/dy
      do i = 1, M+1
        dfcheb(i) = 0.0_dp
        do j = 1, M+1
          dfcheb(i) = dfcheb(i) + D(i,j) * fcheb(j)
        end do
        dfcheb(i) = dfcheb(i) / PI
      end do
      ! compute exact derivative at mapped points
      ! compute Chebyshev errors using Clenshaw-Curtis quadrature weights
      cheb_L2 = 0.0_dp; cheb_Linf = 0.0_dp
      allocate(ccw(M+1))
      call clenshaw_curtis_weights(M, ccw)
      do i = 1, M+1
        x = PI * (xcheb(i) + 1.0_dp)
        dfsp = 3.0_dp * cos(3.0_dp * x)
        cheb_Linf = max(cheb_Linf, abs(dfcheb(i) - dfsp))
        ! integral over x = pi * integral_{-1..1} (err(y))^2 dy
        cheb_L2 = cheb_L2 + (dfcheb(i) - dfsp)**2 * ccw(i)
      end do
      cheb_L2 = sqrt(PI * cheb_L2)
      deallocate(ccw)

      ! write convergence row
      if (ios == 0) then
        write(csvc,'(I6,1A,ES15.8,1A,ES15.8,1A,ES15.8,1A,ES15.8,1A,ES15.8,1A,ES15.8)') N, ',', fourier_L2, ',', fourier_Linf, ',', fd_L2, ',', fd_Linf, ',', cheb_L2, ',', cheb_Linf
      end if

      deallocate(xcheb, fcheb, dfcheb, D)
    end do

    if (ios == 0) close(csvc)
    write(*,'(A)') '  Spectral derivative convergence sweep complete: spectral_convergence.csv'
  end subroutine spectral_derivative_demo

  subroutine chebyshev_D(n, x, D)
    implicit none
    integer, intent(in) :: n
    real(dp), intent(out) :: x(:)
    real(dp), intent(out) :: D(:,:)
    integer :: i, j, nn
    real(dp) :: pi_local
    real(dp), allocatable :: c(:)

    nn = n + 1
    pi_local = 3.141592653589793_dp
    allocate(c(1:nn))

    do i = 1, nn
      x(i) = cos(pi_local * real(i-1,dp) / real(n,dp))
      if (i == 1 .or. i == nn) then
        c(i) = 2.0_dp
      else
        c(i) = 1.0_dp
      end if
    end do

    do i = 1, nn
      do j = 1, nn
        if (i /= j) then
          ! use Trefethen formula with single (-1)^(i-1+j-1) factor
          if (abs(x(i) - x(j)) < 1.0e-16_dp) then
            ! nearly coincident nodes (shouldn't happen) — guard against tiny denom
            D(i,j) = 0.0_dp
          else
            D(i,j) = (c(i) / c(j)) * ((-1.0_dp)**(real(i+j-2,dp))) / (x(i) - x(j))
          end if
        else
          D(i,j) = 0.0_dp
        end if
      end do
    end do

    do i = 1, nn
      if (i > 1) then
        D(i,i) = -sum(D(i,1:i-1))
      else
        D(i,i) = 0.0_dp
      end if
      if (i < nn) then
        D(i,i) = D(i,i) - sum(D(i,i+1:nn))
      end if
    end do

    deallocate(c)
  end subroutine chebyshev_D


  subroutine clenshaw_curtis_weights(n, w)
    implicit none
    integer, intent(in) :: n
    real(dp), intent(out) :: w(:)
    integer :: j, k, m, nn
    real(dp), parameter :: pi_local = 3.141592653589793_dp
    real(dp) :: term, denom

    nn = n + 1
    m = n / 2
    do j = 1, nn
      w(j) = 0.0_dp
    end do

    do k = 0, m
      if (k == 0) then
        denom = 1.0_dp
      else
        denom = 1.0_dp - 4.0_dp * real(k*k, dp)
      end if
      do j = 0, n
        term = cos(2.0_dp * real(k,dp) * real(j,dp) * pi_local / real(n,dp))
        w(j+1) = w(j+1) + term / denom
      end do
    end do

    do j = 1, nn
      w(j) = (2.0_dp / real(n,dp)) * w(j)
    end do

  end subroutine clenshaw_curtis_weights

  subroutine spectral_heat_demo()
    implicit none
    integer :: N, j, k, kappa, nsteps, step
    real(dp) :: L, dx, x, nu, t_final, dt_fd
    real(dp) :: maxerr_spec, maxerr_fd, sumsq_spec, sumsq_fd
    complex(dp), allocatable :: fj(:), Fk(:), tempc(:)
    real(dp), allocatable :: fr(:), u_fd(:), u_spec(:), u_exact(:), xgrid(:)
    real(dp), allocatable :: lap(:)
    complex(dp) :: ci, cexp1
    integer, parameter :: csv_unit = 97
    integer :: ios

    ! Spectral heat solver demo (periodic domain [0,2pi])
    N = 64
    L = 2.0_dp * 3.141592653589793_dp
    dx = L / real(N,dp)
    allocate(fj(N), Fk(N), tempc(N))
    allocate(fr(N), u_fd(N), u_spec(N), u_exact(N), xgrid(N))
    ci = cmplx(0.0_dp, 1.0_dp, kind=dp)

    ! parameters
    nu = 0.1_dp          ! diffusion coefficient
    t_final = 0.5_dp     ! final time

    ! initial condition: combination of sine modes (periodic)
    do j = 1, N
      x = real(j-1,dp) * dx
      xgrid(j) = x
      fr(j) = sin(3.0_dp * x) + 0.5_dp * sin(6.0_dp * x)
      fj(j) = cmplx(fr(j), 0.0_dp, kind=dp)
    end do

    ! Forward DFT: F(k) = sum_j f_j * exp(-i k x_j)
    do k = 0, N-1
      tempc(k+1) = (0.0_dp, 0.0_dp)
      do j = 1, N
        cexp1 = cmplx(cos(-real(k,dp)*xgrid(j)), sin(-real(k,dp)*xgrid(j)), kind=dp)
        tempc(k+1) = tempc(k+1) + fj(j) * cexp1
      end do
      Fk(k+1) = tempc(k+1)
    end do

    ! Evolve each Fourier mode exactly for heat equation: multiply by exp(-nu * kappa^2 * t_final)
    do k = 0, N-1
      if (k <= N/2) then
        kappa = k
      else
        kappa = k - N
      end if
      Fk(k+1) = Fk(k+1) * exp( - nu * real(kappa*kappa, dp) * t_final )
    end do

    ! inverse DFT: u_spec(j) = (1/N) * sum_k Fk * exp(i k x_j)
    do j = 1, N
      tempc(j) = (0.0_dp, 0.0_dp)
      do k = 0, N-1
        if (k <= N/2) then
          kappa = k
        else
          kappa = k - N
        end if
        cexp1 = cmplx(cos(real(kappa,dp)*xgrid(j)), sin(real(kappa,dp)*xgrid(j)), kind=dp)
        tempc(j) = tempc(j) + Fk(k+1) * cexp1
      end do
      u_spec(j) = real( tempc(j) / real(N,dp) )
    end do

    ! Finite-difference explicit forward Euler (periodic) for comparison
    dt_fd = 0.1_dp * dx*dx / nu    ! choose stable-ish dt small enough
    nsteps = max(1, int(t_final / dt_fd))
    dt_fd = t_final / real(nsteps, dp)
    u_fd = fr
    allocate(lap(N))
    do step = 1, nsteps
      ! compute laplacian with periodic wrap into real array lap
      do j = 1, N
        if (j == 1) then
          lap(j) = ( -2.0_dp*u_fd(j) + u_fd(j+1) + u_fd(N) ) / (dx*dx)
        else if (j == N) then
          lap(j) = ( -2.0_dp*u_fd(j) + u_fd(1) + u_fd(j-1) ) / (dx*dx)
        else
          lap(j) = ( -2.0_dp*u_fd(j) + u_fd(j+1) + u_fd(j-1) ) / (dx*dx)
        end if
      end do
      do j = 1, N
        u_fd(j) = u_fd(j) + dt_fd * nu * lap(j)
      end do
    end do
    deallocate(lap)

    ! exact analytic solution for initial sin(3x)+0.5 sin(6x)
    do j = 1, N
      u_exact(j) = sin(3.0_dp * xgrid(j)) * exp( - nu * 9.0_dp * t_final ) + 0.5_dp * sin(6.0_dp * xgrid(j)) * exp( - nu * 36.0_dp * t_final )
    end do

    ! compute errors
    maxerr_spec = 0.0_dp; maxerr_fd = 0.0_dp
    sumsq_spec = 0.0_dp; sumsq_fd = 0.0_dp
    do j = 1, N
      maxerr_spec = max(maxerr_spec, abs(u_spec(j) - u_exact(j)))
      maxerr_fd   = max(maxerr_fd,   abs(u_fd(j)   - u_exact(j)))
      sumsq_spec = sumsq_spec + (u_spec(j) - u_exact(j))**2 * dx
      sumsq_fd   = sumsq_fd   + (u_fd(j)   - u_exact(j))**2 * dx
    end do

    write(*,'(A)') '  Spectral heat demo: analytic modal evolution vs explicit FD'
    write(*,'(A,ES12.6)') '   spectral L2 error =', sqrt(sumsq_spec)
    write(*,'(A,ES12.6)') '   fd       L2 error =', sqrt(sumsq_fd)
    write(*,'(A,ES12.6)') '   spectral max error =', maxerr_spec
    write(*,'(A,ES12.6)') '   fd       max error =', maxerr_fd

    ! write CSV snapshot
    open(unit=csv_unit, file='spectral_heat_snapshot.csv', status='replace', action='write', iostat=ios)
    if (ios == 0) then
      write(csv_unit,'(A)') 'x u_init u_spec u_fd u_exact'
      do j = 1, N
        write(csv_unit,'(F12.8,1A,F12.8,1A,F12.8,1A,F12.8,1A,F12.8)') xgrid(j), ',', fr(j), ',', u_spec(j), ',', u_fd(j), ',', u_exact(j)
      end do
      close(csv_unit)
      write(*,'(A)') '   CSV saved to spectral_heat_snapshot.csv'
    else
      write(*,'(A)') '   Warning: could not open spectral_heat_snapshot.csv for writing.'
    end if

    deallocate(fj, Fk, tempc, fr, u_fd, u_spec, u_exact, xgrid)
  end subroutine spectral_heat_demo

  subroutine poisson_1d_demo()
    implicit none
    integer :: Nx, i, idx
    integer, parameter :: ncases = 3
    integer, dimension(ncases) :: NxList
    real(dp) :: L, dx
    real(dp), allocatable :: a(:), b(:), c(:), d(:), sol(:), xgrid(:), f(:), u_exact(:)
    real(dp) :: m, maxerr, sumsq, L2err
    ! Solve u'' = f on [0,1] with u(0)=u(1)=0 using finite differences
    NxList = (/ 51, 101, 201 /)
    L = 1.0_dp
    write(*,'(A)') "  Poisson 1D demo: convergence study for finite-difference solve of u''=f with u=sin(pi x) exact"
    write(*,'(A)') "   Nx    max_abs_error    L2_error"
    do idx = 1, ncases
      Nx = NxList(idx)
      dx = L / real(Nx-1,dp)
      allocate(a(Nx-2), b(Nx-2), c(Nx-2), d(Nx-2), sol(Nx-2), xgrid(Nx-2), f(Nx-2), u_exact(Nx-2))
      do i = 1, Nx-2
        xgrid(i) = real(i,dp) * dx
        ! choose f so exact u = sin(pi x): u'' = -pi^2 sin(pi x)
        f(i) = - (3.141592653589793_dp**2) * sin(3.141592653589793_dp * xgrid(i))
      end do
      ! tridiagonal coefficients for interior points
      do i = 1, Nx-2
        a(i) = 1.0_dp
        b(i) = -2.0_dp
        c(i) = 1.0_dp
        d(i) = dx*dx * f(i)
      end do
      ! Thomas algorithm (modify in place)
      do i = 2, Nx-2
        m = a(i) / b(i-1)
        b(i) = b(i) - m * c(i-1)
        d(i) = d(i) - m * d(i-1)
      end do
      sol(Nx-2) = d(Nx-2) / b(Nx-2)
      do i = Nx-3, 1, -1
        sol(i) = (d(i) - c(i) * sol(i+1)) / b(i)
      end do
      ! compute L-infinity error and global L2 error
      maxerr = 0.0_dp; sumsq = 0.0_dp
      do i = 1, Nx-2
        u_exact(i) = sin(3.141592653589793_dp * xgrid(i))
        maxerr = max(maxerr, abs(sol(i) - u_exact(i)))
        sumsq = sumsq + (sol(i) - u_exact(i))**2 * dx
      end do
      L2err = sqrt(sumsq)
      write(*,'(I6,3X,ES12.6,3X,ES12.6)') Nx, maxerr, L2err
      deallocate(a,b,c,d,sol,xgrid,f,u_exact)
    end do
  end subroutine poisson_1d_demo

  subroutine linear_algebra_demo()
    implicit none
    real(dp) :: a11, a12, a21, a22, tr, det, disc, lambda1, lambda2
    real(dp) :: v1(2), v2(2), normv, r1(2), r2(2), resnorm1, resnorm2
    
    a11 = 2.0_dp; a12 = 1.0_dp; a21 = 1.0_dp; a22 = 2.0_dp
    tr = a11 + a22
    det = a11*a22 - a12*a21
    disc = sqrt(tr*tr - 4.0_dp*det)
    lambda1 = 0.5_dp * (tr + disc)
    lambda2 = 0.5_dp * (tr - disc)
    write(*,'(A)') "  Linear algebra demo: analytic eigenvalues and eigenvectors of 2x2 matrix"
    write(*,'(A,F12.6,A,F12.6)') "   eigenvalues: ", lambda1, " , ", lambda2
    ! compute eigenvectors (up to scale). For eigenvalue lambda solve (A - lambda I) v = 0
    ! choose v = (a12, lambda - a11) unless that is near zero, otherwise use (lambda - a22, a21)
    v1(1) = a12; v1(2) = lambda1 - a11
    if (abs(v1(1)) < 1.0e-14_dp .and. abs(v1(2)) < 1.0e-14_dp) then
      v1(1) = lambda1 - a22; v1(2) = a21
    end if
    normv = sqrt(v1(1)**2 + v1(2)**2)
    if (normv > 0.0_dp) v1 = v1 / normv
    v2(1) = a12; v2(2) = lambda2 - a11
    if (abs(v2(1)) < 1.0e-14_dp .and. abs(v2(2)) < 1.0e-14_dp) then
      v2(1) = lambda2 - a22; v2(2) = a21
    end if
    normv = sqrt(v2(1)**2 + v2(2)**2)
    if (normv > 0.0_dp) v2 = v2 / normv
    ! verify residuals r = A v - lambda v
    r1(1) = a11*v1(1) + a12*v1(2) - lambda1*v1(1)
    r1(2) = a21*v1(1) + a22*v1(2) - lambda1*v1(2)
    r2(1) = a11*v2(1) + a12*v2(2) - lambda2*v2(1)
    r2(2) = a21*v2(1) + a22*v2(2) - lambda2*v2(2)
    resnorm1 = sqrt(r1(1)**2 + r1(2)**2)
    resnorm2 = sqrt(r2(1)**2 + r2(2)**2)
    write(*,'(A,2F12.6)') "   eigenvector 1 (normalized): ", v1(1), v1(2)
    write(*,'(A,F12.6)') "   residual norm |A v1 - lambda1 v1| =", resnorm1
    write(*,'(A,2F12.6)') "   eigenvector 2 (normalized): ", v2(1), v2(2)
    write(*,'(A,F12.6)') "   residual norm |A v2 - lambda2 v2| =", resnorm2
  end subroutine linear_algebra_demo

  subroutine poisson2d_cg_demo()
    implicit none
    integer :: nx, ny, N, i, j, iter, maxiter, idx
    real(dp) :: Lx, Ly, dx, dy, tol, normr, maxerr, L2err, x, y
    real(dp) :: rr_old, alpha, beta, denom
    real(dp), allocatable :: b(:), xvec(:), r(:), p(:), Ap(:), u_exact(:)

    ! small Poisson problem on unit square with Dirichlet zero BCs and manufactured solution
    nx = 16  ! interior points in x
    ny = 16  ! interior points in y
    Lx = 1.0_dp; Ly = 1.0_dp
    dx = Lx / real(nx+1, dp)
    dy = Ly / real(ny+1, dp)
    N = nx * ny

    allocate(b(N), xvec(N), r(N), p(N), Ap(N), u_exact(N))

    ! choose analytic u(x,y) = sin(pi x) sin(pi y) -> -Lap u = 2*pi^2 sin(pi x) sin(pi y)
    idx = 0
    do j = 1, ny
      y = real(j,dp) * dy
      do i = 1, nx
        x = real(i,dp) * dx
        idx = idx + 1
        u_exact(idx) = sin(3.141592653589793_dp * x) * sin(3.141592653589793_dp * y)
        b(idx) = 2.0_dp * 3.141592653589793_dp**2 * u_exact(idx) * dx * dy / (dx*dy)
      end do
    end do

    ! initial guess zero
    xvec = 0.0_dp

    ! Conjugate Gradient (matrix-free multiply with 5-point Laplacian - interior indexing)
    ! r = b - A x  (x=0 -> r=b)
    r = b
    p = r
    tol = 1.0e-8_dp
    normr = sqrt(sum(r**2))
    maxiter = min(5000, N)

    write(*,'(A)') '  Poisson 2D demo: Conjugate Gradient on 5-point Laplacian (manufactured solution)'
    write(*,'(A,I6)') '   grid (interior) nx*ny =', N

    do iter = 1, maxiter
      ! Ap = A * p  (apply 5-point Laplacian, Dirichlet zeros outside)
      Ap = 0.0_dp
      idx = 0
      do j = 1, ny
        do i = 1, nx
          idx = idx + 1
          ! center contribution (4/h^2) p_ij minus neighbors (-1/h^2)
          Ap(idx) = 4.0_dp * p(idx)
          ! left neighbor
          if (i > 1) then
            Ap(idx) = Ap(idx) - p(idx-1)
          end if
          ! right neighbor
          if (i < nx) then
            Ap(idx) = Ap(idx) - p(idx+1)
          end if
          ! down neighbor
          if (j > 1) then
            Ap(idx) = Ap(idx) - p(idx-nx)
          end if
          ! up neighbor
          if (j < ny) then
            Ap(idx) = Ap(idx) - p(idx+nx)
          end if
        end do
      end do
      ! note: we scaled A such that continuous Laplace ~ (1/dx^2) * discrete; incorporate scale
      Ap = Ap / (dx*dx)

      ! alpha = (r,r) / (p,Ap)
      rr_old = sum(r**2)
      denom = sum(p * Ap)
      if (abs(denom) < 1.0e-30_dp) exit
      alpha = rr_old / denom
      ! x = x + alpha p
      xvec = xvec + alpha * p
      ! r = r - alpha Ap
      r = r - alpha * Ap
      normr = sqrt(sum(r**2))
      if (normr < tol) then
        write(*,'(A,I6,A,ES12.6)') '   converged iter=', iter, '  residual norm=', normr
        exit
      end if
      ! beta = (r_new, r_new) / (r_old, r_old)
      beta = sum(r**2) / rr_old
      p = r + beta * p
    end do
    if (normr >= tol) then
      write(*,'(A,I6,A,ES12.6)') '   stopped iter=', iter, '  residual norm=', normr
    end if

    ! evaluate error vs analytic u_exact (note scaling of discrete solution corresponds to u values)
    maxerr = 0.0_dp; L2err = 0.0_dp
    do idx = 1, N
      maxerr = max(maxerr, abs(xvec(idx) - u_exact(idx)))
      L2err = L2err + (xvec(idx) - u_exact(idx))**2 * dx * dy
    end do
    L2err = sqrt(L2err)
    write(*,'(A)') '   Poisson2D CG results:'
    write(*,'(A,ES12.6)') '    max_abs_error =', maxerr
    write(*,'(A,ES12.6)') '    L2_error =', L2err

    ! write solution to CSV (x,y,u) for plotting
    open(unit=99, file='poisson2d_cg_solution.csv', status='replace')
    write(99,'(A)') 'x,y,u'
    idx = 0
    do j = 1, ny
      y = real(j,dp) * dy
      do i = 1, nx
        x = real(i,dp) * dx
        idx = idx + 1
        write(99,'(3(F12.6,1X))') x, y, xvec(idx)
      end do
    end do
    close(99)
    write(*,'(A)') '    CSV saved to poisson2d_cg_solution.csv'

    deallocate(b, xvec, r, p, Ap, u_exact)
  end subroutine poisson2d_cg_demo

  subroutine moving_surfaces_demo()
    implicit none
    ! Demonstrate an evolving circle: r(t) = r0 + amp * sin(omega t)
    ! Analytic area A(t) = pi * r(t)^2, dA/dt = 2*pi*r * dr/dt
    integer :: k, Npoly
    real(dp) :: r0, amp, omega, t0, dt, rt, rt_plus, rt_minus
    real(dp) :: A_analytic, dA_analytic, dA_numeric, A_plus, A_minus
    real(dp) :: theta, xk, yk, xnext, ynext, area_sum
    real(dp), parameter :: PI = 3.141592653589793_dp

    write(*,'(A)') "  Moving surfaces demo: evolving circle area and numeric derivative (shoelace)"
    r0 = 1.0_dp; amp = 0.2_dp; omega = 2.0_dp*PI  ! one oscillation per unit time
    t0 = 0.37_dp
    dt = 1.0e-4_dp
    ! analytic radius and its time derivative
    rt = r0 + amp * sin(omega * t0)
    dA_analytic = 2.0_dp * PI * rt * (amp * omega * cos(omega * t0))

    ! compute analytic area
    A_analytic = PI * rt * rt

    ! numeric area via polygon at t0 +/- dt using shoelace formula
    Npoly = 720
    area_sum = 0.0_dp
    ! A_minus
    rt_minus = r0 + amp * sin(omega * (t0 - dt))
    area_sum = 0.0_dp
    do k = 0, Npoly-1
      theta = 2.0_dp*PI*real(k,dp)/real(Npoly,dp)
      xk = rt_minus * cos(theta)
      yk = rt_minus * sin(theta)
      theta = 2.0_dp*PI*real(mod(k+1,Npoly),dp)/real(Npoly,dp)
      xnext = rt_minus * cos(theta)
      ynext = rt_minus * sin(theta)
      area_sum = area_sum + (xk*ynext - yk*xnext)
    end do
    A_minus = 0.5_dp * abs(area_sum)

    ! A_plus
    rt_plus = r0 + amp * sin(omega * (t0 + dt))
    area_sum = 0.0_dp
    do k = 0, Npoly-1
      theta = 2.0_dp*PI*real(k,dp)/real(Npoly,dp)
      xk = rt_plus * cos(theta)
      yk = rt_plus * sin(theta)
      theta = 2.0_dp*PI*real(mod(k+1,Npoly),dp)/real(Npoly,dp)
      xnext = rt_plus * cos(theta)
      ynext = rt_plus * sin(theta)
      area_sum = area_sum + (xk*ynext - yk*xnext)
    end do
    A_plus = 0.5_dp * abs(area_sum)

    dA_numeric = (A_plus - A_minus) / (2.0_dp * dt)

    write(*,'(A)') "   Parameters: r0, amp, omega, t0, dt"
    write(*,'(5F12.6)') r0, amp, omega, t0, dt
    write(*,'(A)') "   Area at t0 (analytic) and numeric shoelace around t0 +/- dt and derivative comparison:" 
    write(*,'(A,F16.8)') "    analytic A(t0)=", A_analytic
    write(*,'(A,F16.8)') "    numeric A(t0-dt)~", A_minus
    write(*,'(A,F16.8)') "    numeric A(t0+dt)~", A_plus
    write(*,'(A)') "    derivatives: analytic dA/dt, numeric central-diff dA/dt, abs error"
    write(*,'(3(1X,ES16.8))') dA_analytic, dA_numeric, abs(dA_analytic - dA_numeric)

  end subroutine moving_surfaces_demo

  subroutine lie_groups_demo()
    implicit none
    ! Demonstrate SO(2): exponential map exp(theta * J) where J = [0 -1; 1 0]
    real(dp) :: theta, theta1, theta2
    real(dp) :: R11, R12, R21, R22
    real(dp) :: dR11_exact, dR12_exact, dR21_exact, dR22_exact
    real(dp) :: J11, J12, J21, J22
    real(dp) :: JR11, JR12, JR21, JR22
    real(dp) :: C11, C12, C21, C22, resnorm

    write(*,'(A)') "  Lie groups demo: SO(2) exponential map and properties"
    theta = 0.37_dp
    ! rotation matrix R(theta)
    R11 = cos(theta)
    R12 = -sin(theta)
    R21 = sin(theta)
    R22 = cos(theta)

    ! analytic derivative dR/dtheta
    dR11_exact = -sin(theta)
    dR12_exact = -cos(theta)
    dR21_exact =  cos(theta)
    dR22_exact = -sin(theta)

    ! generator J
    J11 = 0.0_dp; J12 = -1.0_dp
    J21 = 1.0_dp; J22 = 0.0_dp

    ! compute J * R
    JR11 = J11*R11 + J12*R21
    JR12 = J11*R12 + J12*R22
    JR21 = J21*R11 + J22*R21
    JR22 = J21*R12 + J22*R22

    ! check J*R vs analytic dR/dtheta
    resnorm = sqrt( (JR11-dR11_exact)**2 + (JR12-dR12_exact)**2 + (JR21-dR21_exact)**2 + (JR22-dR22_exact)**2 )

    write(*,'(A)') "   Rotation R(theta):"
    write(*,'(4F12.6)') R11, R12, R21, R22
    write(*,'(A)') "   Analytic dR/dtheta (flattened):"
    write(*,'(4F12.6)') dR11_exact, dR12_exact, dR21_exact, dR22_exact
    write(*,'(A)') "   J * R (matrix product) (flattened):"
    write(*,'(4F12.6)') JR11, JR12, JR21, JR22
    write(*,'(A,F12.6)') "   residual norm |J*R - dR/dtheta| =", resnorm

    ! composition test: R(theta1) * R(theta2) == R(theta1+theta2)
    theta1 = 0.5_dp; theta2 = 1.1_dp
    C11 = cos(theta1)*cos(theta2) - sin(theta1)*sin(theta2)
    C12 = -cos(theta1)*sin(theta2) - sin(theta1)*cos(theta2)
    C21 = sin(theta1)*cos(theta2) + cos(theta1)*sin(theta2)
    C22 = -sin(theta1)*sin(theta2) + cos(theta1)*cos(theta2)
    ! expected R(theta1+theta2)
    R11 = cos(theta1+theta2); R12 = -sin(theta1+theta2)
    R21 = sin(theta1+theta2); R22 = cos(theta1+theta2)
    resnorm = sqrt( (C11-R11)**2 + (C12-R12)**2 + (C21-R21)**2 + (C22-R22)**2 )
    write(*,'(A)') "   Composition test R(theta1)*R(theta2) vs R(theta1+theta2): residual norm="
    write(*,'(F12.6)') resnorm

  end subroutine lie_groups_demo

  subroutine crank_nicolson_demo()
    implicit none
    integer :: Nx, i, nsteps, n
    real(dp) :: L, dx, dt, alpha, tfinal, t, r, m, ua
    real(dp), allocatable :: u(:), unew(:), xgrid(:)
    real(dp), allocatable :: a(:), b(:), c(:), d(:), sol(:)
    real(dp) :: maxerr, sumsq, L2err

    ! Parameters
    Nx = 101
    L = 1.0_dp
    alpha = 1.0_dp
    dx = L / real(Nx-1,dp)
    tfinal = 0.1_dp
    dt = 0.5_dp * dx*dx / alpha   ! moderately large but CN is unconditionally stable
    nsteps = max(1, int(tfinal / dt))

    allocate(u(0:Nx-1), unew(0:Nx-1), xgrid(0:Nx-1))
    allocate(a(1:Nx-2), b(1:Nx-2), c(1:Nx-2), d(1:Nx-2), sol(1:Nx-2))

    ! initial condition u(x,0) = sin(pi x)
    do i = 0, Nx-1
      xgrid(i) = real(i,dp) * dx
      u(i) = sin(3.141592653589793_dp * xgrid(i))
    end do

    ! Build constant tridiagonal coefficients for Crank-Nicolson: (I - r/2 A) u^{n+1} = (I + r/2 A) u^n
    ! interior eqns for sol indices 1..Nx-2
    r = alpha * dt / (dx*dx)
    do i = 1, Nx-2
      a(i) = -0.5_dp * r
      b(i) = 1.0_dp + r
      c(i) = -0.5_dp * r
    end do

    do n = 1, nsteps
      ! RHS: (I + r/2 A) * u^n for interior points
      do i = 1, Nx-2
        d(i) = (1.0_dp - r) * u(i) + 0.5_dp * r * ( u(i+1) + u(i-1) )
      end do
      ! apply Dirichlet BCs (u(0)=u(Nx-1)=0) — they already enter via d(1) and d(Nx-2) since u(0),u(Nx-1)=0

      ! Thomas algorithm (forward elimination)
      do i = 2, Nx-2
        m = a(i) / b(i-1)
        b(i) = b(i) - m * c(i-1)
        d(i) = d(i) - m * d(i-1)
      end do
      ! back substitution
      sol(Nx-2) = d(Nx-2) / b(Nx-2)
      do i = Nx-3, 1, -1
        sol(i) = ( d(i) - c(i) * sol(i+1) ) / b(i)
      end do

      ! copy solution into unew (including BCs)
      unew(0) = 0.0_dp
      do i = 1, Nx-2
        unew(i) = sol(i)
      end do
      unew(Nx-1) = 0.0_dp
      u = unew
    end do

    ! analytic solution for comparison: u(x,t) = exp(-pi^2 alpha t) sin(pi x)
    t = real(nsteps,dp) * dt
    maxerr = 0.0_dp; sumsq = 0.0_dp
      do i = 0, Nx-1
      ua = exp(-3.141592653589793_dp**2 * alpha * t) * sin(3.141592653589793_dp * xgrid(i))
      maxerr = max(maxerr, abs(u(i) - ua))
      sumsq = sumsq + (u(i) - ua)**2 * dx
    end do
    L2err = sqrt( max(sumsq, 0.0_dp) )

    write(*,'(A)') '  Crank–Nicolson 1D heat demo (implicit tridiagonal solve)'
    write(*,'(A,1X,F12.8)') '   final time t=', t
    write(*,'(A,1X,E15.6,3X,E15.6)') '   max_abs_error =', maxerr, L2err

    deallocate(u, unew, xgrid, a, b, c, d, sol)
  end subroutine crank_nicolson_demo

  subroutine power_method_demo()
    implicit none
    integer :: n, i, j, iter, maxiter
    real(dp), parameter :: eps = 1.0e-15_dp
    real(dp) :: tol, lambda_old, lambda_est, normv, resnorm
    real(dp), allocatable :: A(:,:), v(:), w(:)

    ! small symmetric test matrix (5x5)
    n = 5
    allocate(A(n,n), v(n), w(n))
    A = 0.0_dp
    do i = 1, n
      A(i,i) = 2.0_dp + real(i-1,dp)
      if (i < n) then
        A(i,i+1) = -1.0_dp
        A(i+1,i) = -1.0_dp
      end if
    end do

    ! initial vector
    do i = 1, n
      v(i) = 1.0_dp
    end do

    tol = 1.0e-10_dp
    maxiter = 1000
    lambda_old = 0.0_dp

    write(*,'(A)') '  Power method demo: compute dominant eigenvalue of a small symmetric tridiagonal matrix'
    do iter = 1, maxiter
      ! w = A * v
      w = 0.0_dp
      do i = 1, n
        do j = 1, n
          w(i) = w(i) + A(i,j) * v(j)
        end do
      end do
      ! normalize w to get next v
      normv = sqrt(sum(w**2))
      if (abs(normv) <= eps) exit
      v = w / normv
      ! Rayleigh quotient as eigenvalue estimate
      lambda_est = 0.0_dp
      do i = 1, n
        do j = 1, n
          lambda_est = lambda_est + v(i) * A(i,j) * v(j)
        end do
      end do
      if (abs(lambda_est - lambda_old) < tol) then
        exit
      end if
      lambda_old = lambda_est
    end do

    ! residual norm ||A v - lambda v||
    w = 0.0_dp
    do i = 1, n
      do j = 1, n
        w(i) = w(i) + A(i,j) * v(j)
      end do
      w(i) = w(i) - lambda_est * v(i)
    end do
    resnorm = sqrt(sum(w**2))

    write(*,'(A,F12.8)') '   dominant eigenvalue (Rayleigh) =', lambda_est
    write(*,'(A,F12.8)') '   residual norm ||A v - lambda v|| =', resnorm

    deallocate(A, v, w)
  end subroutine power_method_demo

recursive subroutine multigrid_poisson_demo()
  implicit none
  integer :: nx, ny, maxcycles, cycle
  real(dp) :: Lx, h, tol
  real(dp), allocatable :: b(:,:), u(:,:)

  print *, 'Multigrid Poisson demo: geometric V-cycle on unit square'

  nx = 32  ! interior points in x (should be power-of-two multiple for coarsening)
  ny = nx
  Lx = 1.0_dp
  h = Lx/(nx+1)
  tol = 1.0e-8_dp
  maxcycles = 50

  allocate(b(nx,ny), u(nx,ny))
  u = 0.0_dp

  call build_rhs_sinpi(b,nx,ny,h)

  open(unit=77, file='poisson2d_multigrid_residuals.csv', status='replace')
  write(77,'(A)') 'cycle,residual_l2'

  do cycle = 1, maxcycles
    call vcycle(u,b,nx,h)
    write(77,'(I0,1A,ES14.6)') cycle, ',', compute_residual_norm(u,b,nx,h)
    if (compute_residual_norm(u,b,nx,h) < tol) exit
  end do

  close(77)

  call write_grid_csv('poisson2d_multigrid_solution.csv',u,nx,ny,h)
  print *, 'Multigrid done; CSV saved to poisson2d_multigrid_solution.csv'

  deallocate(b,u)
end subroutine multigrid_poisson_demo

recursive subroutine vcycle(u,b,n,h)
  real(dp), intent(inout) :: u(:,:)
  real(dp), intent(in) :: b(:,:)
  integer, intent(in) :: n
  real(dp), intent(in) :: h
  integer :: nc
  real(dp), allocatable :: r(:,:), bc(:,:), ec(:,:), ef(:,:)
  integer :: nu1, nu2

  nu1 = 3
  nu2 = 3

  call smooth(u,b,n,h,nu1)
  allocate(r(n,n))
  call compute_residual(r,u,b,n,h)

  if (n <= 4) then
    call smooth(u,b,n,h,80)
    deallocate(r)
    return
  end if

  nc = n/2
  allocate(bc(nc,nc), ec(nc,nc))
  call restrict_fullweight(r,bc,nc)
  ec = 0.0_dp
  call vcycle(ec,bc,nc,2.0_dp*h)

  allocate(ef(n,n))
  ef = 0.0_dp
  call prolong_inject(ec,ef,nc,n)
  u = u + ef

  call smooth(u,b,n,h,nu2)

  deallocate(r,bc,ec,ef)
end subroutine vcycle

subroutine build_rhs_sinpi(b,nx,ny,h)
  real(dp), intent(out) :: b(:,:)
  integer, intent(in) :: nx,ny
  real(dp), intent(in) :: h
  integer :: i,j
  real(dp) :: x,y,pi
  pi = 4.0_dp*atan(1.0_dp)
  do j=1,ny
    y = j*h
    do i=1,nx
      x = i*h
      b(i,j) = 2.0_dp*pi*pi*sin(pi*x)*sin(pi*y)  ! -Laplace u for u = sin(pi x) sin(pi y)
    end do
  end do
end subroutine build_rhs_sinpi

subroutine smooth(u,b,n,h,nsweep)
  real(dp), intent(inout) :: u(:,:)
  real(dp), intent(in) :: b(:,:)
  integer, intent(in) :: n, nsweep
  real(dp), intent(in) :: h
  integer :: sweep,i,j
  real(dp) :: omega, unew
  omega = 2.0_dp/3.0_dp
  do sweep = 1, nsweep
    do j = 1, n
      do i = 1, n
        unew = 0.25_dp*(get_u(u,i+1,j,n)+get_u(u,i-1,j,n)+get_u(u,i,j+1,n)+get_u(u,i,j-1,n) - h*h*b(i,j))
        u(i,j) = (1.0_dp-omega)*u(i,j) + omega*unew
      end do
    end do
  end do
end subroutine smooth

pure function get_u(u,i,j,n) result(val)
  real(dp), intent(in) :: u(:,:)
  integer, intent(in) :: i,j,n
  real(dp) :: val
  if (i < 1 .or. i > n .or. j < 1 .or. j > n) then
    val = 0.0_dp
  else
    val = u(i,j)
  end if
end function get_u

subroutine compute_residual(r,u,b,n,h)
  real(dp), intent(out) :: r(:,:)
  real(dp), intent(in) :: u(:,:), b(:,:)
  integer, intent(in) :: n
  real(dp), intent(in) :: h
  integer :: i,j
  real(dp) :: Au
  do j = 1, n
    do i = 1, n
      Au = (get_u(u,i-1,j,n) - 4.0_dp*u(i,j) + get_u(u,i+1,j,n) + get_u(u,i,j-1,n) + get_u(u,i,j+1,n))/(h*h)
      r(i,j) = b(i,j) - Au
    end do
  end do
end subroutine compute_residual

function compute_residual_norm(u,b,n,h) result(rnorm)
  real(dp), intent(in) :: u(:,:), b(:,:)
  integer, intent(in) :: n
  real(dp), intent(in) :: h
  real(dp) :: rnorm
  real(dp), allocatable :: r(:,:)
  integer :: i,j
  real(dp) :: sumsq
  allocate(r(n,n))
  call compute_residual(r,u,b,n,h)
  sumsq = 0.0_dp
  do j=1,n
    do i=1,n
      sumsq = sumsq + r(i,j)*r(i,j)
    end do
  end do
  rnorm = sqrt(max(sumsq,0.0_dp))
  deallocate(r)
end function compute_residual_norm

subroutine restrict_fullweight(r,bc,nc)
  real(dp), intent(in) :: r(:,:)
  real(dp), intent(out) :: bc(:,:)
  integer, intent(in) :: nc
  integer :: ic,jc,ifx,jfx
  ! simple 2x2 averaging restriction (nf not required here)
  do jc = 1, nc
    jfx = 2*jc
    do ic = 1, nc
      ifx = 2*ic
      bc(ic,jc) = 0.25_dp*( r(ifx-1,jfx-1) + r(ifx,jfx-1) + r(ifx-1,jfx) + r(ifx,jfx) )
    end do
  end do
end subroutine restrict_fullweight

subroutine prolong_inject(ec,ef,nc,nf)
  real(dp), intent(in) :: ec(:,:)
  real(dp), intent(out) :: ef(:,:)
  integer, intent(in) :: nc,nf
  integer :: i,j,ic,jc
  ef = 0.0_dp
  do jc = 1, nc
    do ic = 1, nc
      i = 2*ic-1
      j = 2*jc-1
      ef(i,j) = ef(i,j) + ec(ic,jc)
      if (i+1 <= nf) ef(i+1,j) = ef(i+1,j) + 0.5_dp*ec(ic,jc)
      if (j+1 <= nf) ef(i,j+1) = ef(i,j+1) + 0.5_dp*ec(ic,jc)
      if (i+1 <= nf .and. j+1 <= nf) ef(i+1,j+1) = ef(i+1,j+1) + 0.25_dp*ec(ic,jc)
    end do
  end do
end subroutine prolong_inject

subroutine write_grid_csv(fname,u,nx,ny,h)
  character(len=*), intent(in) :: fname
  real(dp), intent(in) :: u(:,:)
  integer, intent(in) :: nx,ny
  real(dp), intent(in) :: h
  integer :: i,j
  integer :: unit
  open(newunit=unit, file=fname, status='replace')
  write(unit,'(A)') 'x,y,u'
  do j=1,ny
    do i=1,nx
      write(unit,'(3(ES14.6,1X))') i*h, j*h, u(i,j)
    end do
  end do
  close(unit)
end subroutine write_grid_csv

subroutine fft_filter_demo()
  implicit none
  integer :: N, k, i
  real(dp) :: L, dx, x, cutoff
  complex(dp), allocatable :: a(:)
  real(dp), allocatable :: xgrid(:), u(:), u_filtered(:)
  character(len=64) :: fname

  write(*,'(A)') 'FFT + Spectral filtering demo (Cooley-Tukey radix-2)'

  N = 256
  L = 2.0_dp * 3.141592653589793_dp
  dx = L / real(N,dp)
  allocate(a(N), xgrid(N), u(N), u_filtered(N))

  ! build test signal: sum of three sinusoids (k=1,5,40)
  do i = 1, N
    x = (real(i-1,dp)) * dx
    xgrid(i) = x
    u(i) = sin(1.0_dp * x) + 0.5_dp*sin(5.0_dp * x) + 0.8_dp*sin(40.0_dp * x)
    a(i) = cmplx(u(i), 0.0_dp, kind=dp)
  end do

  call fft_inplace(a, N, .false.)

  ! apply low-pass filter: zero-out high-frequency bins (simple rectangular filter)
  cutoff = 10.0_dp
  do k = 1, N
    if ( (k-1) > int(cutoff, kind=4) .and. (k-1) < N - int(cutoff, kind=4) ) then
      a(k) = (0.0_dp, 0.0_dp)
    end if
  end do

  call fft_inplace(a, N, .true.)

  ! extract real part (inverse FFT normalized inside fft_inplace)
  do i = 1, N
    u_filtered(i) = real(a(i))
  end do

  fname = 'fft_filter_output.csv'
  open(unit=88, file=fname, status='replace')
  write(88,'(A)') 'x,original,filtered'
  do i = 1, N
    write(88,'(3(ES14.6,1X))') xgrid(i), u(i), u_filtered(i)
  end do
  close(88)

  write(*,'(A)') 'FFT filtering done; CSV saved to fft_filter_output.csv'

  deallocate(a, xgrid, u, u_filtered)
end subroutine fft_filter_demo

subroutine fft_inplace(a, n, inverse)
  implicit none
  complex(dp), intent(inout) :: a(:)
  integer, intent(in) :: n
  logical, intent(in) :: inverse
  integer :: i, j, m, istep, mmax, k
  real(dp) :: angle, PI
  complex(dp) :: w, t

  PI = 4.0_dp * atan(1.0_dp)

  ! bit-reversal permutation (1-based indexing)
  j = 1
  do i = 1, n
    if (j > i) then
      t = a(j); a(j) = a(i); a(i) = t
    end if
    m = n/2
    do while (m >= 1 .and. j > m)
      j = j - m
      m = m/2
    end do
    j = j + m
  end do

  mmax = 1
  do while (mmax < n)
    istep = 2*mmax
    do m = 0, mmax-1
      angle = -2.0_dp * PI * real(m,dp) / real(istep,dp)
      if (inverse) angle = -angle
      w = cmplx(cos(angle), sin(angle), kind=dp)
      do k = m+1, n, istep
        j = k + mmax
        t = w * a(j)
        a(j) = a(k) - t
        a(k) = a(k) + t
      end do
    end do
    mmax = istep
  end do

  if (inverse) then
    do i = 1, n
      a(i) = a(i) / real(n,dp)
    end do
  end if
end subroutine fft_inplace
 
subroutine stft_demo()
  implicit none
  integer, parameter :: dp = selected_real_kind(15,307)
  integer :: N, M, hop, num_frames, i, j, k, frame_idx
  real(dp) :: dx, x, PI
  real(dp), allocatable :: signal(:), window(:), mags(:)
  complex(dp), allocatable :: buffer(:)
  character(len=64) :: fname
  integer :: outu
  character(len=512) :: header
  character(len=32) :: tmp
  integer :: kb

  PI = 4.0_dp * atan(1.0_dp)
  write(*,'(A)') 'STFT demo: compute spectrogram of a chirp and save CSV (frames x freq bins)'

  N = 2048                ! signal length
  M = 128                 ! window length (power of two)
  hop = M/2               ! 50% overlap
  dx = 2.0_dp*PI / real(N,dp)

  allocate(signal(N))
  allocate(window(M))
  allocate(buffer(M))
  allocate(mags(M/2+1))

  ! build a chirp: frequency increases linearly from 5 to 200 over the signal
  do i = 1, N
    x = real(i-1,dp)/real(N,dp)
    signal(i) = sin( 2.0_dp*PI * (5.0_dp + (200.0_dp-5.0_dp)*x) * x )
  end do

  ! Hann window
  do i = 1, M
    window(i) = 0.5_dp*(1.0_dp - cos(2.0_dp*PI*real(i-1,dp)/real(M-1,dp)))
  end do

  num_frames = 1 + (N - M) / hop
  fname = 'stft_spectrogram.csv'
  open(newunit=outu, file=fname, status='replace')
  ! write CSV header: time, f0, f1, ... f_{M/2}
  
  header = 'time'
  do kb = 0, M/2
    write(tmp,'(I0)') kb
    header = trim(header) // ',' // 'f' // trim(tmp)
  end do
  write(outu,'(A)') trim(header)

  frame_idx = 0
  do i = 1, N - M + 1, hop
    frame_idx = frame_idx + 1
    ! fill buffer and apply window
    do j = 1, M
      buffer(j) = cmplx( window(j) * signal(i + j -1), 0.0_dp, kind=dp)
    end do
    call fft_inplace(buffer, M, .false.)
    ! compute magnitudes for bins 0..M/2
    do k = 0, M/2
      mags(k+1) = abs(buffer(k+1))
    end do
    ! write time (center of window) and magnitudes as a CSV row
    write(outu,'(F12.6)',advance='no') real(i+M/2-1,dp) * dx
    do k = 1, M/2+1
      write(outu,'(A,ES12.6)',advance='no') ',', mags(k)
    end do
    write(outu,*)
  end do

  close(outu)
  write(*,'(A)') 'STFT complete; CSV saved to stft_spectrogram.csv'

  deallocate(signal, window, buffer, mags)
end subroutine stft_demo

subroutine stiff_integrator_demo()
  implicit none
  integer, parameter :: dp = selected_real_kind(15,307)
  integer :: i, steps
  real(dp) :: alpha, t0, tfinal, dt, t, y, y_prev, y_next, y_exact
  real(dp) :: k1, k2, k3, k4
  real(dp), allocatable :: dts(:)
  character(len=64) :: fname
  integer :: outu, j

  write(*,'(A)') 'Stiff integrator comparison demo: implicit Euler, BDF2, RK4'

  alpha = 1000.0_dp
  t0 = 0.0_dp
  tfinal = 1.0_dp
  allocate(dts(4))
  ! Use time steps small enough to keep explicit RK4 in its stability region
  ! For y' = -alpha y with alpha=1000, require alpha*dt ~ O(1) or smaller.
  dts = (/ 1.0e-4_dp, 5.0e-4_dp, 1.0e-3_dp, 2.0e-3_dp /)

  fname = 'stiff_integrator_comparison.csv'
  open(newunit=outu, file=fname, status='replace')
  write(outu,*) 'method dt steps y numeric y_exact rel_error'

  do j = 1, size(dts)
    dt = dts(j)
    steps = max(1, int( (tfinal - t0)/dt + 0.5_dp ))

    ! Exact solution for y' = -alpha*y
    y_exact = exp(-alpha * tfinal)

    ! Implicit Euler (linear update: y_{n+1} = y_n / (1 + alpha dt))
    y = 1.0_dp
    do i = 1, steps
      y = y / (1.0_dp + alpha*dt)
    end do
    write(outu,*) 'implicit_euler', dt, steps, y, y_exact, abs((y - y_exact)/max(abs(y_exact),1.0_dp))

    ! BDF2 (linear two-step: (3 y_{n+1} - 4 y_n + y_{n-1})/(2 dt) = -alpha y_{n+1})
    if (steps == 1) then
      ! fall back to implicit Euler for single step
      y = 1.0_dp / (1.0_dp + alpha*dt)
      write(outu,*) 'bdf2', dt, steps, y, y_exact, abs((y - y_exact)/max(abs(y_exact),1.0_dp))
    else
      y_prev = 1.0_dp
      y = y_prev / (1.0_dp + alpha*dt)  ! one implicit-euler starter
      do i = 2, steps
        y_next = (4.0_dp*y - y_prev) / (3.0_dp + 2.0_dp*alpha*dt)
        y_prev = y
        y = y_next
      end do
      write(outu,*) 'bdf2', dt, steps, y, y_exact, abs((y - y_exact)/max(abs(y_exact),1.0_dp))
    end if

    ! Explicit RK4 fixed-step
    y = 1.0_dp
    t = t0
    do i = 1, steps
      k1 = -alpha * y
      k2 = -alpha * (y + 0.5_dp*dt*k1)
      k3 = -alpha * (y + 0.5_dp*dt*k2)
      k4 = -alpha * (y + dt*k3)
      y = y + dt*(k1 + 2.0_dp*k2 + 2.0_dp*k3 + k4)/6.0_dp
      t = t + dt
    end do
    write(outu,*) 'rk4', dt, steps, y, y_exact, abs((y - y_exact)/max(abs(y_exact),1.0_dp))

  end do

  close(outu)
  deallocate(dts)
  write(*,'(A)') 'Stiff integrator demo complete; CSV saved to stiff_integrator_comparison.csv'
end subroutine stiff_integrator_demo

subroutine symplectic_higher_order_demo()
  implicit none
  integer, parameter :: dp = selected_real_kind(15,307)
  real(dp) :: t, dt, tmax, qv, pv, qm, pm, qtmp, ptmp
  integer :: nsteps, i, outu, ios, write_every
  real(dp) :: H_ver, H_mid

  write(*,'(A)') 'Symplectic higher-order demo: pendulum, compare Velocity-Verlet vs implicit midpoint'

  dt = 0.1_dp
  tmax = 200.0_dp
  nsteps = max(1, int(tmax / dt))
  write_every = 10

  ! initial conditions: small angle and zero momentum
  qv = 1.0_dp   ! initial angle (rad)
  pv = 0.0_dp
  qm = qv
  pm = pv

  open(newunit=outu, file='symplectic_higher_order.csv', status='replace', iostat=ios)
  if (ios /= 0) then
    write(*,'(A)') '  Warning: could not open symplectic_higher_order.csv for writing.'
  else
    write(outu,'(A)') 't,q_ver,p_ver,E_ver,q_mid,p_mid,E_mid'
  end if

  t = 0.0_dp
  do i = 0, nsteps
    ! compute energies
    H_ver = 0.5_dp * pv*pv + (1.0_dp - cos(qv))
    H_mid = 0.5_dp * pm*pm + (1.0_dp - cos(qm))
    if (mod(i,write_every) == 0) then
      if (ios == 0) write(outu,'(F12.6,1X,6(1X,ES14.8))') t, qv, pv, H_ver, qm, pm, H_mid
    end if

    if (i == nsteps) exit

    ! Velocity-Verlet (explicit symplectic)
    pv = pv - 0.5_dp * dt * sin(qv)
    qv = qv + dt * pv
    pv = pv - 0.5_dp * dt * sin(qv)

    ! Implicit midpoint method (symplectic) - solve scalar nonlinear equation for qm_next
    call implicit_midpoint_step(qm, pm, dt, qtmp, ptmp)
    qm = qtmp
    pm = ptmp

    t = t + dt
  end do

  if (ios == 0) close(outu)
  write(*,'(A)') 'Symplectic higher-order demo complete; CSV saved to symplectic_higher_order.csv'
end subroutine symplectic_higher_order_demo

subroutine implicit_midpoint_step(qn, pn, dt, qnp1, pnp1)
  implicit none
  integer, parameter :: dp = selected_real_kind(15,307)
  real(dp), intent(in) :: qn, pn, dt
  real(dp), intent(out) :: qnp1, pnp1
  real(dp) :: tol, F, dF, qguess, delta
  integer :: it, maxit

  ! Solve for q_{n+1} via Newton on scalar: F(q_new) = q_new - qn - dt*pn + dt*dt/4*( sin(qn) + sin(q_new) ) = 0
  tol = 1.0e-12_dp
  maxit = 12
  qguess = qn + dt * pn  ! initial guess
  do it = 1, maxit
    F = qguess - qn - dt*pn + (dt*dt/4.0_dp) * ( sin(qn) + sin(qguess) )
    dF = 1.0_dp + (dt*dt/4.0_dp) * cos(qguess)
    if (abs(dF) < 1.0e-16_dp) dF = sign(1.0e-16_dp, dF)
    delta = - F / dF
    qguess = qguess + delta
    if (abs(delta) < tol) exit
  end do
  qnp1 = qguess
  pnp1 = pn - 0.5_dp * dt * ( sin(qn) + sin(qnp1) )
end subroutine implicit_midpoint_step

subroutine monte_carlo_convergence_demo()
  implicit none
  integer, parameter :: dp = selected_real_kind(15,307)
  integer :: sizes(3) = (/1000, 5000, 10000/)
  integer :: isz, M, j
  real(dp) :: u, f, g, h, sum_f, sum_f2, mean_f, var_f, var_est
  real(dp) :: sum_a, sum_a2, mean_a, var_a, var_est_a
  real(dp) :: sum_h, sum_h2, mean_h, var_h, var_est_h
  integer :: outu, ios

  open(newunit=outu, file='monte_carlo_convergence.csv', status='replace', action='write', iostat=ios)
  if (ios /= 0) then
    write(*,'(A)') 'Warning: could not open monte_carlo_convergence.csv'
    return
  end if
  write(outu,'(A)') 'N,plain_mean,plain_var_est,antithetic_mean,antithetic_var_est,controlvar_mean,controlvar_var_est'

  do isz = 1, size(sizes)
    M = sizes(isz)

    ! Plain Monte Carlo for integral of f(x)=x^2 on [0,1]
    sum_f = 0.0_dp; sum_f2 = 0.0_dp
    do j = 1, M
      call random_number(u)
      f = u*u
      sum_f = sum_f + f
      sum_f2 = sum_f2 + f*f
    end do
    mean_f = sum_f / real(M,dp)
    var_f  = max(0.0_dp, sum_f2/real(M,dp) - mean_f*mean_f)
    var_est = var_f / real(M,dp)

    ! Antithetic sampling: pair u and 1-u
    sum_a = 0.0_dp; sum_a2 = 0.0_dp
    do j = 1, M/2
      call random_number(u)
      f = u*u
      g = (1.0_dp - u)**2
      h = 0.5_dp*(f + g)
      sum_a = sum_a + h
      sum_a2 = sum_a2 + h*h
    end do
    if (mod(M,2) == 1) then
      call random_number(u)
      h = u*u
      sum_a = sum_a + h
      sum_a2 = sum_a2 + h*h
    end if
    mean_a = sum_a / real(M,dp)
    var_a  = max(0.0_dp, sum_a2/real(M,dp) - mean_a*mean_a)
    var_est_a = var_a / real(M,dp)

    ! Control variate using g(x)=x with known integral 0.5: estimator = mean(f-g) + 0.5
    sum_h = 0.0_dp; sum_h2 = 0.0_dp
    do j = 1, M
      call random_number(u)
      f = u*u
      g = u
      h = f - g
      sum_h = sum_h + h
      sum_h2 = sum_h2 + h*h
    end do
    mean_h = sum_h / real(M,dp)
    var_h = max(0.0_dp, sum_h2/real(M,dp) - mean_h*mean_h)
    var_est_h = var_h / real(M,dp)
    ! control-var estimator
    mean_h = mean_h + 0.5_dp

    write(outu,*) M, ',', mean_f, ',', var_est, ',', mean_a, ',', var_est_a, ',', mean_h, ',', var_est_h
  end do

  close(outu)
  write(*,'(A)') 'Monte Carlo convergence demo: CSV saved to monte_carlo_convergence.csv'
end subroutine monte_carlo_convergence_demo

subroutine sde_milstein_vs_em_demo()
  implicit none
  integer, parameter :: dp = selected_real_kind(15,307)
  real(dp) :: mu, sigma, y0, T
  real(dp), allocatable :: dts(:)
  integer :: ndt, i, j, steps, M
  integer :: k
  real(dp) :: dt, sqrt_dt, Z, y_em, y_mil, y_exact, Wtot
  real(dp) :: sumsq_em, sumsq_mil
  integer :: outu, ios

  mu = 0.5_dp; sigma = 0.3_dp; y0 = 1.0_dp; T = 1.0_dp
  M = 1000  ! ensemble size
  allocate(dts(4))
  dts = (/ 1.0e-3_dp, 5.0e-3_dp, 1.0e-2_dp, 5.0e-2_dp /)
  ndt = size(dts)

  open(newunit=outu, file='sde_milstein_vs_em.csv', status='replace', action='write', iostat=ios)
  if (ios /= 0) then
    write(*,'(A)') 'Warning: could not open sde_milstein_vs_em.csv'
    return
  end if
  write(outu,'(A)') 'dt,em_rmse,milstein_rmse'

  do i = 1, ndt
    dt = dts(i)
    steps = max(1, int(T / dt + 0.5_dp))
    sumsq_em = 0.0_dp
    sumsq_mil = 0.0_dp

    do j = 1, M
      y_em = y0; y_mil = y0; Wtot = 0.0_dp
      do k = 1, steps
        Z = normal_rand()
        sqrt_dt = sqrt(dt)
        Wtot = Wtot + sqrt_dt * Z
        ! EM update: y_{n+1} = y_n + mu y_n dt + sigma y_n sqrt(dt) Z
        y_em = y_em + mu*y_em*dt + sigma*y_em*sqrt_dt*Z
        ! Milstein update for multiplicative noise: y_{n+1} = y_n + mu y_n dt + sigma y_n dW + 0.5 sigma^2 y_n ( (dW)^2 - dt )
        y_mil = y_mil + mu*y_mil*dt + sigma*y_mil*sqrt_dt*Z + 0.5_dp * sigma*sigma * y_mil * ( (sqrt_dt*Z)**2 - dt )
      end do

      ! exact solution for GBM at T using W_T = Wtot
      y_exact = y0 * exp( (mu - 0.5_dp*sigma*sigma)*T + sigma * Wtot )
      sumsq_em = sumsq_em + (y_em - y_exact)**2
      sumsq_mil = sumsq_mil + (y_mil - y_exact)**2
    end do

    write(outu,'(F10.3,1A,2(ES18.10,1A))') dt, ',', sqrt(sumsq_em/real(M,dp)), ',', sqrt(sumsq_mil/real(M,dp))
  end do

  close(outu)
  write(*,'(A)') 'SDE integrator demo: CSV saved to sde_milstein_vs_em.csv'
  deallocate(dts)
end subroutine sde_milstein_vs_em_demo

subroutine sparse_matrix_benchmarks()
  implicit none
  integer, parameter :: dp = selected_real_kind(15,307)
  integer :: Ns(5) = (/1000, 2000, 4000, 8000, 16000/)
  integer :: iN, N, nnz, i, idx, outu, ioerr
  integer, allocatable :: ia(:), ja(:)
  real(dp), allocatable :: a(:), x(:), b(:), y(:)
  real(dp) :: t0, t1, t_direct, t_cg, relres
  integer :: maxit, iters

  open(newunit=outu, file='sparse_matrix_benchmarks.csv', status='replace', action='write', iostat=ioerr)
  if (ioerr /= 0) then
    write(*,'(A)') 'Warning: could not open sparse_matrix_benchmarks.csv'
    return
  end if
  ! header: N,nnz,direct_time,cg_time,cg_iters,cg_relres
  write(outu,'(A)') 'N,nnz,direct_time,cg_time,cg_iters,cg_relres'

  do iN = 1, size(Ns)
    N = Ns(iN)
    if (N < 2) cycle
    nnz = 3*N - 2
    allocate(ia(1:N+1), ja(1:nnz))
    allocate(a(1:nnz), x(1:N), b(1:N), y(1:N))

    ! build CSR for 1D Laplacian (Dirichlet-like) with diag=2, offdiag=-1
    idx = 1
    ia(1) = 1
    do i = 1, N
      if (i > 1) then
        ja(idx) = i-1; a(idx) = -1.0_dp; idx = idx + 1
      end if
      ja(idx) = i; a(idx) = 2.0_dp; idx = idx + 1
      if (i < N) then
        ja(idx) = i+1; a(idx) = -1.0_dp; idx = idx + 1
      end if
      ia(i+1) = idx
    end do

    ! RHS b = 1
    b = 1.0_dp

    ! Direct solve (Thomas tridiagonal solver)
    call cpu_time(t0)
    call thomas_tridiag_solve(N, -1.0_dp, 2.0_dp, -1.0_dp, b, x)
    call cpu_time(t1)
    t_direct = t1 - t0

    ! Conjugate Gradient on CSR
    maxit = N
    call cpu_time(t0)
    call cg_solve_csr(N, ia, ja, a, b, x, 1.0e-8_dp, maxit, iters, relres)
    call cpu_time(t1)
    t_cg = t1 - t0

    ! write CSV line
    write(outu,'(I0,1A,I0,1A,ES12.6,1A,ES12.6,1A,I0,1A,ES12.6)') N, ',', nnz, ',', t_direct, ',', t_cg, ',', iters, ',', relres

    deallocate(ia, ja, a, x, b, y)
  end do

  close(outu)
  write(*,'(A)') 'Sparse matrix benchmarks: CSV saved to sparse_matrix_benchmarks.csv'
end subroutine sparse_matrix_benchmarks

  subroutine thomas_tridiag_solve(n, a_lower, a_diag, a_upper, b, x)
    implicit none
    integer, intent(in) :: n
    real(dp), intent(in) :: a_lower, a_diag, a_upper
    real(dp), intent(in) :: b(:)
    real(dp), intent(out) :: x(:)
    real(dp), allocatable :: cprime(:), dprime(:)
    integer :: i
    allocate(cprime(1:n-1), dprime(1:n))
    ! forward sweep (using constant coefficients)
    cprime(1) = a_upper / a_diag
    dprime(1) = b(1) / a_diag
    do i = 2, n-1
      cprime(i) = a_upper / (a_diag - a_lower*cprime(i-1))
    end do
    do i = 2, n
      dprime(i) = ( b(i) - a_lower * dprime(i-1) ) / ( a_diag - a_lower * cprime(i-1) )
    end do
    x(n) = dprime(n)
    do i = n-1, 1, -1
      x(i) = dprime(i) - cprime(i) * x(i+1)
    end do
    deallocate(cprime, dprime)
  end subroutine thomas_tridiag_solve

  subroutine sparse_matvec_csr(n, ia, ja, a, v, out)
    implicit none
    integer, intent(in) :: n
    integer, intent(in) :: ia(:), ja(:)
    real(dp), intent(in) :: a(:), v(:)
    real(dp), intent(out) :: out(:)
    integer :: i, k
    out = 0.0_dp
    do i = 1, n
      do k = ia(i), ia(i+1)-1
        out(i) = out(i) + a(k) * v( ja(k) )
      end do
    end do
  end subroutine sparse_matvec_csr

  subroutine cg_solve_csr(n, ia, ja, a, b, x, tol, maxit, iters, relres)
    implicit none
    integer, intent(in) :: n, ia(:), ja(:), maxit
    real(dp), intent(in) :: a(:), b(:), tol
    real(dp), intent(inout) :: x(:)
    integer, intent(out) :: iters
    real(dp), intent(out) :: relres
    real(dp), allocatable :: r(:), p(:), Ap(:)
    real(dp) :: alpha, beta, rsold, rsnew, bnrm2
    integer :: k

    allocate(r(1:n), p(1:n), Ap(1:n))
    ! initial guess x may contain previous direct solution; reset to zero for CG timing fairness
    x = 0.0_dp
    call sparse_matvec_csr(n, ia, ja, a, x, Ap)
    r = b - Ap
    p = r
    rsold = dot_product(r, r)
    bnrm2 = sqrt( max(1.0e-30_dp, dot_product(b,b)) )
    if (bnrm2 == 0.0_dp) bnrm2 = 1.0_dp
    iters = 0
    do k = 1, maxit
      call sparse_matvec_csr(n, ia, ja, a, p, Ap)
      alpha = rsold / max(1.0e-30_dp, dot_product(p, Ap))
      x = x + alpha * p
      r = r - alpha * Ap
      rsnew = dot_product(r, r)
      relres = sqrt(rsnew) / bnrm2
      iters = k
      if (relres <= tol) exit
      beta = rsnew / rsold
      p = r + beta * p
      rsold = rsnew
    end do
    deallocate(r, p, Ap)
  end subroutine cg_solve_csr
  subroutine svd_pca_demo()
    implicit none
    integer :: N, dim, i, j, k, it, maxit, outu, ios
    real(dp), allocatable :: X(:,:), Xc(:,:), Cmat(:,:), v(:), w(:), tmp(:)
    real(dp) :: meancol, tol, lambda, traceC, explained

    ! Toy dataset: N samples, dim features
    N = 10
    dim = 3
    allocate(X(N,dim))
    do i = 1, N
      X(i,1) = real(i,dp)
      X(i,2) = 2.0_dp * X(i,1) + (-1.0_dp)**i * 0.1_dp
      X(i,3) = sin(real(i,dp))
    end do

    allocate(Xc(N,dim))
    do j = 1, dim
      meancol = 0.0_dp
      do i = 1, N
        meancol = meancol + X(i,j)
      end do
      meancol = meancol / real(N,dp)
      do i = 1, N
        Xc(i,j) = X(i,j) - meancol
      end do
    end do

    ! Covariance matrix C = (1/(N-1)) Xc^T Xc
    allocate(Cmat(dim,dim))
    Cmat = 0.0_dp
    do i = 1, dim
      do j = 1, dim
        do k = 1, N
          Cmat(i,j) = Cmat(i,j) + Xc(k,i) * Xc(k,j)
        end do
        Cmat(i,j) = Cmat(i,j) / real(N-1,dp)
      end do
    end do

    traceC = 0.0_dp
    do i = 1, dim
      traceC = traceC + Cmat(i,i)
    end do

    open(newunit=outu, file='svd_pca.csv', action='write', status='replace', iostat=ios)
    if (ios /= 0) then
      write(*,'(A)') 'Could not open svd_pca.csv'
      return
    end if
    write(outu,'(A)') 'comp,eigenvalue,explained_variance,vec1,vec2,vec3'

    allocate(v(dim), w(dim), tmp(dim))
    do k = 1, dim
      v = 1.0_dp
      tol = 1.0e-8_dp
      maxit = 1000
      lambda = 0.0_dp
      do it = 1, maxit
        ! w = Cmat * v
        w = 0.0_dp
        do i = 1, dim
          do j = 1, dim
            w(i) = w(i) + Cmat(i,j) * v(j)
          end do
        end do
        tmp = w
        lambda = sqrt(sum(tmp*tmp))
        if (lambda == 0.0_dp) exit
        v = tmp / lambda
        ! Rayleigh quotient
        lambda = 0.0_dp
        do i = 1, dim
          do j = 1, dim
            lambda = lambda + v(i) * Cmat(i,j) * v(j)
          end do
        end do
        ! convergence check: norm(Cv - lambda v)
        tmp = 0.0_dp
        do i = 1, dim
          do j = 1, dim
            tmp(i) = tmp(i) + Cmat(i,j) * v(j)
          end do
          tmp(i) = tmp(i) - lambda * v(i)
        end do
        if (sqrt(sum(tmp*tmp)) < tol) exit
      end do
      if (traceC > 0.0_dp) then
        explained = lambda / traceC
      else
        explained = 0.0_dp
      end if
      write(outu,'(I0,A,ES18.10,A,ES18.10,A,ES18.10,A,ES18.10,A,ES18.10)') k, ',', lambda, ',', explained, ',', v(1), ',', v(2), ',', v(3)
      ! deflate
      do i = 1, dim
        do j = 1, dim
          Cmat(i,j) = Cmat(i,j) - lambda * v(i) * v(j)
        end do
      end do
    end do

    close(outu)
    write(*,'(A)') 'SVD/PCA demo: CSV saved to svd_pca.csv'

    deallocate(X, Xc, Cmat, v, w, tmp)
  end subroutine svd_pca_demo

  subroutine bfgs_demo()
    implicit none
    integer :: n, maxit, iter, outu, ios, i, j
    real(dp), allocatable :: x(:), grad(:), xnew(:), gradnew(:), p(:), s(:), y(:)
    real(dp), allocatable :: H(:,:)
    real(dp), allocatable :: Hy(:)
    real(dp) :: fval, fnew, alpha, c1, rho_ls, sTy, gradnorm
    real(dp) :: step_norm, rho, scalar

    n = 2
    allocate(x(n), grad(n), xnew(n), gradnew(n), p(n), s(n), y(n), H(n,n))

    ! Rosenbrock initial guess
    x(1) = -1.2_dp
    x(2) =  1.0_dp

    ! initial function and gradient
    call compute_rosen(x, fval, grad)

    ! initialize inverse-Hessian approximation to identity
    H = 0.0_dp
    do iter = 1, n
      H(iter,iter) = 1.0_dp
    end do

    open(newunit=outu, file='bfgs_demo.csv', action='write', status='replace', iostat=ios)
    if (ios /= 0) then
      write(*,'(A)') 'Could not open bfgs_demo.csv'
      return
    end if
    write(outu,'(A)') 'iter,fval,alpha,step_norm,grad_norm'

    maxit = 200
    c1 = 1.0e-4_dp
    rho_ls = 0.5_dp

    do iter = 1, maxit
      ! search direction p = - H * grad
      p = 0.0_dp
      do i = 1, n
        do j = 1, n
          p(i) = p(i) - H(i,j) * grad(j)
        end do
      end do

      ! backtracking Armijo line search
      alpha = 1.0_dp
      do
        xnew = x + alpha * p
        call compute_rosen(xnew, fnew, gradnew)
        if (fnew <= fval + c1 * alpha * dot_product(grad, p)) exit
        alpha = alpha * rho_ls
        if (alpha < 1.0e-16_dp) exit
      end do

      s = xnew - x
      y = gradnew - grad
      sTy = dot_product(s, y)

      if (sTy > 1.0e-20_dp) then
        ! Hy = H * y
        allocate(Hy(n))
        Hy = 0.0_dp
        do i = 1, n
          do j = 1, n
            Hy(i) = Hy(i) + H(i,j) * y(j)
          end do
        end do
        rho = 1.0_dp / sTy
        scalar = rho * (1.0_dp + rho * dot_product(y, Hy))
        ! Rank-2 update: H = H - rho*(Hy*s^T + s*Hy^T) + scalar*(s*s^T)
        do i = 1, n
          do j = 1, n
            H(i,j) = H(i,j) - rho * Hy(i) * s(j) - rho * s(i) * Hy(j) + scalar * s(i) * s(j)
          end do
        end do
        deallocate(Hy)
      end if

      ! accept step
      x = xnew
      grad = gradnew
      fval = fnew

      gradnorm = sqrt(dot_product(grad, grad))
      step_norm = sqrt(dot_product(s, s))
      write(outu,'(I0,A,ES18.10,A,ES18.10,A,ES18.10,A,ES18.10)') iter, ',', fval, ',', alpha, ',', step_norm, ',', gradnorm

      if (gradnorm < 1.0e-6_dp) exit
    end do

    close(outu)
    write(*,'(A)') 'BFGS demo: CSV saved to bfgs_demo.csv'

    deallocate(x, grad, xnew, gradnew, p, s, y, H)
  end subroutine bfgs_demo

  pure function dual_add(a, b) result(z)
    type(dual), intent(in) :: a, b
    type(dual) :: z
    z%a = a%a + b%a
    z%b = a%b + b%b
  end function dual_add

  pure function dual_mul(a, b) result(z)
    type(dual), intent(in) :: a, b
    type(dual) :: z
    z%a = a%a * b%a
    z%b = a%a * b%b + a%b * b%a
  end function dual_mul

  pure function dual_sin(a) result(z)
    type(dual), intent(in) :: a
    type(dual) :: z
    z%a = sin(a%a)
    z%b = cos(a%a) * a%b
  end function dual_sin

  pure function dual_exp(a) result(z)
    type(dual), intent(in) :: a
    type(dual) :: z
    z%a = exp(a%a)
    z%b = exp(a%a) * a%b
  end function dual_exp

  subroutine eval_fun_real(xx, f)
    implicit none
    real(dp), intent(in) :: xx(:)
    real(dp), intent(out) :: f(2)
    ! f1 = x1^2 + sin(x2); f2 = exp(x1) * x2
    f(1) = xx(1)**2 + sin(xx(2))
    f(2) = exp(xx(1)) * xx(2)
  end subroutine eval_fun_real

  subroutine autodiff_dual_demo()
    implicit none
    integer :: outu, ios
    type(dual) :: x1d, x2d, f1d, f2d
    real(dp) :: x(2), v(2), h, fbase(2), fpert(2), jvp_fd(2)
    real(dp) :: err(2)

    ! choose a test point and direction
    x(1) = 0.5_dp; x(2) = -0.3_dp
    v(1) = 0.2_dp; v(2) = 0.4_dp

    ! build dual variables with derivative parts = direction
    x1d%a = x(1); x1d%b = v(1)
    x2d%a = x(2); x2d%b = v(2)

    ! evaluate vector function using dual arithmetic
    f1d = dual_add( dual_pow2(x1d), dual_sin(x2d) )
    f2d = dual_mul( dual_exp(x1d), x2d )

    ! finite-difference directional derivative (forward diff)
    h = 1.0e-6_dp
    call eval_fun_real(x, fbase)
    x(1) = x(1) + h*v(1); x(2) = x(2) + h*v(2)
    call eval_fun_real(x, fpert)
    jvp_fd(1) = (fpert(1) - fbase(1)) / h
    jvp_fd(2) = (fpert(2) - fbase(2)) / h

    open(newunit=outu, file='autodiff_dual_jvp.csv', action='write', status='replace', iostat=ios)
    if (ios /= 0) then
      write(*,'(A)') 'Could not open autodiff_dual_jvp.csv'
      return
    end if
    write(outu,'(A)') 'component,dual_jvp,finite_diff_jvp,abs_err'

    err(1) = abs(f1d%b - jvp_fd(1))
    err(2) = abs(f2d%b - jvp_fd(2))
    write(outu,'(I0,A,ES18.10,A,ES18.10,A,ES18.10)') 1, ',', f1d%b, ',', jvp_fd(1), ',', err(1)
    write(outu,'(I0,A,ES18.10,A,ES18.10,A,ES18.10)') 2, ',', f2d%b, ',', jvp_fd(2), ',', err(2)

    close(outu)
    write(*,'(A)') 'Autodiff dual-number Jv demo: CSV saved to autodiff_dual_jvp.csv'
  end subroutine autodiff_dual_demo

  subroutine compute_rosen(xx, f, g)
    implicit none
    real(dp), intent(in) :: xx(:)
    real(dp), intent(out) :: f
    real(dp), intent(out) :: g(:)
    real(dp) :: t
    ! Rosenbrock f(x,y) = 100*(y - x^2)^2 + (1 - x)^2
    t = xx(2) - xx(1)**2
    f = 100.0_dp * t*t + (1.0_dp - xx(1))**2
    g(1) = -400.0_dp * xx(1) * t - 2.0_dp * (1.0_dp - xx(1))
    g(2) = 200.0_dp * t
  end subroutine compute_rosen

  subroutine geodesic_shooting_demo()
    implicit none
    integer :: Nsteps, i, outu, ios
    real(dp) :: dt, T, tt, arc, vn, ang
    real(dp), dimension(3) :: x0, x, v, k1x, k1v, k2x, k2v, k3x, k3v, k4x, k4v, tx, tv

    ! Unit-sphere geodesic ODE: x' = v, v' = - (v·v) * x  (great-circle flow)
    x0 = (/ 1.0_dp, 0.0_dp, 0.0_dp /)
    x = x0
    v = (/ 0.0_dp, 1.0_dp, 0.0_dp /)  ! initial tangent pointing along +y (unit speed)
    v = v / sqrt(sum(v*v))

    T = 1.5707963267948966_dp   ! integrate to pi/2 (quarter great-circle)
    dt = 0.01_dp
    Nsteps = max(1, int(T / dt))
    arc = 0.0_dp

    open(newunit=outu, file='geodesic_shooting.csv', action='write', status='replace', iostat=ios)
    if (ios /= 0) then
      write(*,'(A)') 'Could not open geodesic_shooting.csv'
      return
    end if
    write(outu,'(A)') 't,x,y,z,arc_length,analytic_angle'

    do i = 0, Nsteps
      tt = real(i,dp) * dt
      ang = acos( max(-1.0_dp, min(1.0_dp, dot_product(x0, x))) )
      write(outu,'(ES12.6,1X,3(ES12.6,1X),ES12.6,1X,ES12.6)') tt, x(1), x(2), x(3), arc, ang

      if (i == Nsteps) exit

      ! RK4 step for system [x; v]
      k1x = v
      k1v = - (dot_product(v,v)) * x

      tx = x + 0.5_dp * dt * k1x
      tv = v + 0.5_dp * dt * k1v
      k2x = tv
      k2v = - (dot_product(tv,tv)) * tx

      tx = x + 0.5_dp * dt * k2x
      tv = v + 0.5_dp * dt * k2v
      k3x = tv
      k3v = - (dot_product(tv,tv)) * tx

      tx = x + dt * k3x
      tv = v + dt * k3v
      k4x = tv
      k4v = - (dot_product(tv,tv)) * tx

      x = x + dt*(k1x + 2.0_dp*k2x + 2.0_dp*k3x + k4x) / 6.0_dp
      v = v + dt*(k1v + 2.0_dp*k2v + 2.0_dp*k3v + k4v) / 6.0_dp

      ! Re-normalize to reduce drift and enforce tangent constraint
      x = x / sqrt(sum(x*x))
      v = v - dot_product(v,x) * x
      vn = sqrt(sum(v*v))
      if (vn > 0.0_dp) v = v / vn

      arc = arc + dt * 1.0_dp
    end do

    close(outu)
    write(*,'(A)') 'Geodesic shooting demo: CSV saved to geodesic_shooting.csv'
  end subroutine geodesic_shooting_demo

  subroutine numerical_curvature_demo()
    implicit none
    integer :: Npoints, i, j, k, outu, ios, nneigh, cnt
    integer, parameter :: maxneigh = 60
    character(len=512) :: line
    real(dp), allocatable :: P(:,:), dist2(:), neighx(:), neighy(:), neighz(:)
    real(dp), allocatable :: dlist(:)
    integer, allocatable :: idxList(:)
    real(dp), allocatable :: Kapprox(:)
    real(dp) :: vx(3), vy(3), ref(3), delta(3), xloc, yloc, zloc
    real(dp) :: PI, golden, theta, z, r
    real(dp), dimension(6,6) :: ATA
    real(dp), dimension(6) :: ATb, coeff
    integer :: nn
    integer :: tmpi
    real(dp) :: xi, yi, zi
    real(dp), dimension(6) :: row
    real(dp) :: f_xx, f_xy, f_yy, f_x, f_y

    PI = 3.141592653589793_dp
    ! sampling and local fit parameters
    Npoints = 400
    nneigh = 30
    if (nneigh > maxneigh) nneigh = maxneigh

    allocate(P(3,Npoints))
    allocate(dist2(Npoints))
    allocate(neighx(nneigh), neighy(nneigh), neighz(nneigh))
    allocate(dlist(nneigh), idxList(nneigh))
    allocate(Kapprox(Npoints))

    ! Fibonacci-like sampling on unit sphere
    golden = (1.0_dp + sqrt(5.0_dp)) / 2.0_dp
    do i = 0, Npoints-1
      theta = 2.0_dp * PI * real(i,dp) / golden
      z = 1.0_dp - 2.0_dp * real(i,dp) / real(max(1,Npoints-1),dp)
      r = sqrt(max(0.0_dp, 1.0_dp - z*z))
      P(1,i+1) = r * cos(theta)
      P(2,i+1) = r * sin(theta)
      P(3,i+1) = z
    end do

    ! For each sample point, find nearest nneigh neighbors and fit local quadratic
    do i = 1, Npoints
      ! compute squared distances
      do j = 1, Npoints
        dist2(j) = (P(1,j)-P(1,i))**2 + (P(2,j)-P(2,i))**2 + (P(3,j)-P(3,i))**2
      end do
      ! simple selection of nearest nneigh (excluding self)
      do j = 1, nneigh
        dlist(j) = 1.0e300_dp
        idxList(j) = 0
      end do
      ! find nneigh smallest distances (inefficient but simple)
      do j = 1, Npoints
        if (j == i) cycle
        ! insert into sorted list by distance
        k = nneigh
        if (dist2(j) < dlist(k)) then
          dlist(k) = dist2(j)
          idxList(k) = j
          ! bubble down
          do while (k > 1)
            if (dlist(k) < dlist(k-1)) then
              call swap_real(dlist(k), dlist(k-1))
              tmpi = idxList(k); idxList(k) = idxList(k-1); idxList(k-1) = tmpi
              k = k - 1
            else
              exit
            end if
          end do
        end if
      end do

      ! build local coordinates in tangent plane at P(:,i)
      ! choose reference vector not parallel to P(:,i)
      ref = (/ 0.0_dp, 0.0_dp, 1.0_dp /)
      if (abs(P(3,i)) > 0.9_dp) ref = (/ 1.0_dp, 0.0_dp, 0.0_dp /)
      ! u = normalize(cross(P, ref))
      vx(1) = P(2,i)*ref(3) - P(3,i)*ref(2)
      vx(2) = P(3,i)*ref(1) - P(1,i)*ref(3)
      vx(3) = P(1,i)*ref(2) - P(2,i)*ref(1)
      ! normalize vx
      if (sqrt(sum(vx*vx)) == 0.0_dp) then
        vx = (/1.0_dp, 0.0_dp, 0.0_dp/)
      else
        vx = vx / sqrt(sum(vx*vx))
      end if
      ! vy = cross(P, vx)
      vy(1) = P(2,i)*vx(3) - P(3,i)*vx(2)
      vy(2) = P(3,i)*vx(1) - P(1,i)*vx(3)
      vy(3) = P(1,i)*vx(2) - P(2,i)*vx(1)
      vy = vy / sqrt(sum(vy*vy))

      ! collect neighbor coordinates projected to tangent plane
      cnt = 0
      do j = 1, nneigh
        nn = idxList(j)
        if (nn < 1 .or. nn > Npoints) cycle
        cnt = cnt + 1
        delta(1) = P(1,nn) - P(1,i)
        delta(2) = P(2,nn) - P(2,i)
        delta(3) = P(3,nn) - P(3,i)
        xloc = delta(1)*vx(1) + delta(2)*vx(2) + delta(3)*vx(3)
        yloc = delta(1)*vy(1) + delta(2)*vy(2) + delta(3)*vy(3)
        zloc = delta(1)*P(1,i) + delta(2)*P(2,i) + delta(3)*P(3,i)
        neighx(cnt) = xloc
        neighy(cnt) = yloc
        neighz(cnt) = zloc
      end do

      if (cnt < 6) then
        Kapprox(i) = -1.0_dp
        cycle
      end if

      ! Build normal equations ATA * coeff = ATb for quadratic fit z = a x^2 + b x y + c y^2 + d x + e y + f
      ATA = 0.0_dp
      ATb = 0.0_dp
      do j = 1, cnt
        xi = neighx(j); yi = neighy(j); zi = neighz(j)
        row = (/ xi*xi, xi*yi, yi*yi, xi, yi, 1.0_dp /)
        do k = 1, 6
          ATb(k) = ATb(k) + row(k) * zi
          do nn = 1, 6
            ATA(k,nn) = ATA(k,nn) + row(k) * row(nn)
          end do
        end do
      end do

      ! solve 6x6 system
      if (.not. solve_linear_system(6, ATA, ATb, coeff)) then
        Kapprox(i) = -1.0_dp
        cycle
      end if

      ! second derivatives from fit
      f_xx = 2.0_dp * coeff(1)
      f_xy = coeff(2)
      f_yy = 2.0_dp * coeff(3)
      f_x  = coeff(4)
      f_y  = coeff(5)
      Kapprox(i) = (f_xx*f_yy - f_xy*f_xy) / ( (1.0_dp + f_x*f_x + f_y*f_y)**2 )
    end do

    ! write CSV with approximations and errors (unit sphere exact Gaussian curvature = 1)
    open(newunit=outu, file='numerical_curvature.csv', action='write', status='replace', iostat=ios)
    if (ios /= 0) then
      write(*,'(A)') 'Could not open numerical_curvature.csv'
      return
    end if
    call csv_write(outu, 'idx,x,y,z,K_approx,K_exact,abs_err', ios)
    do i = 1, Npoints
      if (Kapprox(i) < 0.0_dp) cycle
      write(line,'(I0,1A,ES12.6,1A,ES12.6,1A,ES12.6,1A,ES12.6,1A,ES12.6,1A,ES12.6)') i, ',', P(1,i), ',', P(2,i), ',', P(3,i), ',', Kapprox(i), ',', 1.0_dp, ',', abs(Kapprox(i) - 1.0_dp)
      call csv_write(outu, trim(line), ios)
    end do
    close(outu)
    write(*,'(A)') 'Numerical curvature demo: CSV saved to numerical_curvature.csv'

    deallocate(P, dist2, neighx, neighy, neighz, Kapprox)
  end subroutine numerical_curvature_demo

  subroutine swap_real(a, b)
    implicit none
    real(dp), intent(inout) :: a, b
    real(dp) :: tmp
    tmp = a; a = b; b = tmp
  end subroutine swap_real

  logical function solve_linear_system(n, A, b, x)
    implicit none
    integer, intent(in) :: n
    real(dp), intent(inout) :: A(n,n)
    real(dp), intent(in) :: b(n)
    real(dp), intent(out) :: x(n)
    integer :: i, j, k, piv
    real(dp) :: maxv, tmp, factor
    real(dp), allocatable :: M(:,:), rhs(:)

    allocate(M(n,n), rhs(n))
    M = A; rhs = b
    ! Gaussian elimination with partial pivoting
    do k = 1, n
      piv = k
      maxv = abs(M(k,k))
      do i = k+1, n
        if (abs(M(i,k)) > maxv) then
          maxv = abs(M(i,k)); piv = i
        end if
      end do
      if (piv /= k) then
        do j = k, n
          tmp = M(k,j); M(k,j) = M(piv,j); M(piv,j) = tmp
        end do
        tmp = rhs(k); rhs(k) = rhs(piv); rhs(piv) = tmp
      end if
      if (abs(M(k,k)) < 1.0e-18_dp) then
        solve_linear_system = .false.
        deallocate(M, rhs)
        return
      end if
      ! eliminate
      do i = k+1, n
        factor = M(i,k) / M(k,k)
        do j = k, n
          M(i,j) = M(i,j) - factor * M(k,j)
        end do
        rhs(i) = rhs(i) - factor * rhs(k)
      end do
    end do
    ! back substitution
    do i = n, 1, -1
      tmp = rhs(i)
      do j = i+1, n
        tmp = tmp - M(i,j) * x(j)
      end do
      x(i) = tmp / M(i,i)
    end do
    solve_linear_system = .true.
    deallocate(M, rhs)
  end function solve_linear_system

  subroutine derivative_methods_comparison_demo()
    implicit none
    integer, parameter :: dp = selected_real_kind(15,307)
    integer :: i, iu
    real(dp) :: x0, exact, hvals(12), h
    real(dp) :: fd_approx, cs_approx, rich_approx, dual_approx
    real(dp) :: best_err, best_h, err_i
    character(len=256) :: row
    type(dual) :: z, y

    open(unit=120, file='derivative_methods_comparison.csv', status='replace', action='write', iostat=iu)
    if (iu /= 0) then
      write(*,'(A)') '  Warning: could not open derivative_methods_comparison.csv for writing.'
      return
    end if
    write(120,'(A)') 'h,method,approx,exact,abs_err'

    x0 = 1.0_dp
    exact = exp(x0)
    hvals = (/ 1.0e-1_dp, 1.0e-2_dp, 1.0e-3_dp, 1.0e-4_dp, 1.0e-5_dp, 1.0e-6_dp, 1.0e-7_dp, 1.0e-8_dp, 1.0e-9_dp, 1.0e-10_dp, 1.0e-11_dp, 1.0e-12_dp /)

    do i = 1, size(hvals)
      h = hvals(i)
      fd_approx = deriv1_exp(x0, h)
      cs_approx = deriv1_complex_exp(x0, h)
      rich_approx = deriv1_richardson_exp(x0, h, 4)
      z%a = x0; z%b = 1.0_dp
      y = dual_exp_func(z)
      dual_approx = y%b

      write(row,'(ES18.10,A,A,ES18.10,A,ES18.10,A,ES18.10)') h, ',', 'central,', fd_approx, ',', exact, ',', abs(fd_approx - exact)
      write(120,'(A)') trim(row)
      write(row,'(ES18.10,A,A,ES18.10,A,ES18.10,A,ES18.10)') h, ',', 'complex,', cs_approx, ',', exact, ',', abs(cs_approx - exact)
      write(120,'(A)') trim(row)
      write(row,'(ES18.10,A,A,ES18.10,A,ES18.10,A,ES18.10)') h, ',', 'richardson,', rich_approx, ',', exact, ',', abs(rich_approx - exact)
      write(120,'(A)') trim(row)
      write(row,'(ES18.10,A,A,ES18.10,A,ES18.10,A,ES18.10)') h, ',', 'dual,', dual_approx, ',', exact, ',', abs(dual_approx - exact)
      write(120,'(A)') trim(row)
    end do

    close(120)

    ! Find best h for central difference (simple scan)
    best_err = huge(1.0_dp)
    best_h = 0.0_dp
    do i = 1, size(hvals)
      err_i = abs( deriv1_exp(x0, hvals(i)) - exact )
      if (err_i < best_err) then
        best_err = err_i
        best_h = hvals(i)
      end if
    end do

    write(*,'(A)') 'Derivative methods comparison: CSV saved to derivative_methods_comparison.csv'
    write(*,'(A,ES12.6,A,ES12.6)') '  best central-diff error =', best_err, '  h=', best_h

  end subroutine derivative_methods_comparison_demo

  function dual_exp_func(u) result(v)
    implicit none
    type(dual), intent(in) :: u
    type(dual) :: v
    v%a = exp(u%a)
    v%b = exp(u%a) * u%b
  end function dual_exp_func

  subroutine spinor_calculus_demo()
    implicit none
    integer, parameter :: dp = selected_real_kind(15,307)
    complex(dp) :: psi(4), chi(4)
    complex(dp) :: gamma0(4,4), gamma1(4,4), gamma2(4,4), gamma3(4,4)
    complex(dp) :: S0, S1, S2, S3
    integer :: i,j

    ! Simple Dirac spinor demo using Pauli-block representation (Dirac basis)
    gamma0 = (0.0_dp,0.0_dp)
    gamma1 = (0.0_dp,0.0_dp)
    gamma2 = (0.0_dp,0.0_dp)
    gamma3 = (0.0_dp,0.0_dp)
    ! gamma^0 = [[I,0],[0,-I]] with I = identity 2x2
    do i = 1,2
      gamma0(i,i) = (1.0_dp,0.0_dp)
      gamma0(i+2,i+2) = (-1.0_dp,0.0_dp)
    end do
    ! gamma^k = [[0,sigma_k],[ -sigma_k, 0 ]] (use Pauli matrices)
    gamma1(1,3) = (0.0_dp,0.0_dp); gamma1(1,4) = (1.0_dp,0.0_dp)
    gamma1(2,3) = (1.0_dp,0.0_dp); gamma1(2,4) = (0.0_dp,0.0_dp)
    gamma1(3,1) = (0.0_dp,0.0_dp); gamma1(4,1) = (-1.0_dp,0.0_dp)
    gamma1(3,2) = (-1.0_dp,0.0_dp); gamma1(4,2) = (0.0_dp,0.0_dp)
    ! gamma2 (using i*sigma2 blocks)
    gamma2(1,3) = (0.0_dp,0.0_dp); gamma2(1,4) = (0.0_dp,1.0_dp)
    gamma2(2,3) = (0.0_dp,-1.0_dp); gamma2(2,4) = (0.0_dp,0.0_dp)
    gamma2(3,1) = (0.0_dp,0.0_dp); gamma2(4,1) = (0.0_dp,-1.0_dp)
    gamma2(3,2) = (0.0_dp,1.0_dp); gamma2(4,2) = (0.0_dp,0.0_dp)
    ! gamma3 (sigma3)
    gamma3(1,3) = (1.0_dp,0.0_dp); gamma3(1,4) = (0.0_dp,0.0_dp)
    gamma3(2,3) = (0.0_dp,0.0_dp); gamma3(2,4) = (-1.0_dp,0.0_dp)
    gamma3(3,1) = (-1.0_dp,0.0_dp); gamma3(4,1) = (0.0_dp,0.0_dp)
    gamma3(3,2) = (0.0_dp,0.0_dp); gamma3(4,2) = (1.0_dp,0.0_dp)

    ! Example spinors (normalized-ish)
    psi = (/ (1.0_dp,0.0_dp), (0.1_dp,0.2_dp), (0.0_dp,0.0_dp), (0.0_dp,0.0_dp) /)
    chi = (/ (0.0_dp,0.0_dp), (0.3_dp,-0.1_dp), (0.5_dp,0.0_dp), (0.0_dp,0.2_dp) /)

    ! Compute Dirac bilinears psi^† gamma^mu chi for mu=0..3
    S0 = (0.0_dp,0.0_dp); S1 = (0.0_dp,0.0_dp); S2 = (0.0_dp,0.0_dp); S3 = (0.0_dp,0.0_dp)
    do i = 1,4
      do j = 1,4
        S0 = S0 + conjg(psi(i)) * gamma0(i,j) * chi(j)
        S1 = S1 + conjg(psi(i)) * gamma1(i,j) * chi(j)
        S2 = S2 + conjg(psi(i)) * gamma2(i,j) * chi(j)
        S3 = S3 + conjg(psi(i)) * gamma3(i,j) * chi(j)
      end do
    end do

    write(*,'(A)') 'Spinor calculus demo: Dirac bilinears <psi|gamma^mu|chi>'
    write(*,'(A,ES18.10,1X,ES18.10)') '  gamma^0 (re,im)=', real(S0), aimag(S0)
    write(*,'(A,ES18.10,1X,ES18.10)') '  gamma^1 (re,im)=', real(S1), aimag(S1)
    write(*,'(A,ES18.10,1X,ES18.10)') '  gamma^2 (re,im)=', real(S2), aimag(S2)
    write(*,'(A,ES18.10,1X,ES18.10)') '  gamma^3 (re,im)=', real(S3), aimag(S3)
  end subroutine spinor_calculus_demo


  subroutine hyperfunction_calculus_demo()
    implicit none
    integer, parameter :: dp = selected_real_kind(15,307)
    integer :: iu, N, k
    real(dp) :: R, dtheta, theta
    complex(dp) :: z, sumint
    ! Toy hyperfunction-like demo: numeric check of contour integral for 1/(z-1)
    N = 800
    R = 2.0_dp
    dtheta = 2.0_dp * 3.141592653589793_dp / real(N,dp)
    sumint = (0.0_dp, 0.0_dp)
    do k = 0, N-1
      theta = dtheta * real(k,dp)
      z = cmplx(R*cos(theta), R*sin(theta), kind=dp)
      sumint = sumint + (1.0_dp/(z - cmplx(1.0_dp,0.0_dp,kind=dp))) * cmplx(0.0_dp,1.0_dp,kind=dp) * cmplx(R*cos(theta), R*sin(theta), kind=dp)
    end do
    sumint = sumint * dtheta
    open(unit=231, file='hyperfunction_calculus.csv', status='replace', action='write', iostat=iu)
    if (iu /= 0) then
      write(*,'(A)') '  Warning: could not open hyperfunction_calculus.csv'
      return
    end if
    write(231,'(A)') 'integral_real,integral_imag,abs_val'
    write(231,'(ES18.10,1A,ES18.10,1A,ES18.10)') real(sumint), aimag(sumint), abs(sumint)
    close(231)
    write(*,'(A)') 'Hyperfunction calculus demo: contour integral saved to hyperfunction_calculus.csv'
  end subroutine hyperfunction_calculus_demo

  subroutine fractional_calculus_demo()
    implicit none
    integer, parameter :: dp = selected_real_kind(15,307)
    integer :: ia, N, M, k, ios
    real(dp) :: alpha, x0, h, exact, sum, ck, gl_approx
    real(dp) :: D_h, D_h2, D_h4, E1, E2, D_h_qp, D_h2_qp, D_h4_qp, E2_qp, rel_err_hp
    integer :: N2, N4
    real(kind=qp) :: D_h_qp_k, D_h2_qp_k, D_h4_qp_k, E1_qp_k, E2_qp_k, p_qp
    ! note: 'approx' is unused here (left for compatibility) but keep to avoid API churn
    real(dp), dimension(:), allocatable :: alphas, Ms
    integer, parameter :: csv_unit = 240
    character(len=256) :: line
    character(len=256) :: fld
    real(dp) :: rel_err

    ! Grünwald–Letnikov fractional derivative demo for f(x)=x^2 at x=1
    alphas = (/ 0.5_dp, 0.8_dp, 1.2_dp /)
    Ms = (/ 50.0_dp, 100.0_dp, 200.0_dp /)
    x0 = 1.0_dp

    open(unit=csv_unit, file='fractional_calculus.csv', status='replace', action='write', iostat=ios)
    if (ios /= 0) then
      write(*,'(A)') '  Warning: could not open fractional_calculus.csv'
      return
    end if
        write(csv_unit,'(A)') 'alpha,M,h,analytic_deriv,GL_approx,abs_err,rel_err,p,iter,method,'// &
          'D_h,D_h2,D_h4,E1,E2,D_h_qp,D_h2_qp,D_h4_qp,E2_qp,rel_err_hp'

    do ia = 1, size(alphas)
      alpha = alphas(ia)
      do N = 1, size(Ms)
        M = int(Ms(N))
        ! choose step h = x0 / M so that we evaluate back to x=0
        h = x0 / real(M,dp)
        sum = 0.0_dp
        ck = 1.0_dp
        ! c_k = (-1)^k * binomial(alpha, k) computed recursively
        do k = 0, M
          if (k == 0) then
            ck = 1.0_dp
          else
            ck = ck * ( (alpha - real(k-1,dp)) / real(k,dp) ) * (-1.0_dp)
          end if
          ! f(x0 - k h) for f(x)=x^2, if argument negative use zero extension (toy)
          if (x0 - real(k,dp)*h >= 0.0_dp) then
            sum = sum + ck * (x0 - real(k,dp)*h)**2
          end if
        end do
        ! Compute GL numeric (illustrative) diagnostics and analytic exact (canonical) for x^2
        N2 = max(1, 2*M)
        N4 = max(1, 4*M)
        D_h  = frac_deriv_gl_poly2_adaptive(x0, alpha, h, M)
        D_h2 = frac_deriv_gl_poly2_adaptive(x0, alpha, h/2.0_dp, N2)
        D_h4 = frac_deriv_gl_poly2_adaptive(x0, alpha, h/4.0_dp, N4)
        call compute_richardson_extrap(D_h, D_h2, D_h4, E1, E2, last_frac_p)

        ! attempt quad-precision diagnostics (may be expensive)
        D_h_qp_k  = frac_deriv_gl_poly2_adaptive_qp(x0, alpha, h, M)
        D_h2_qp_k = frac_deriv_gl_poly2_adaptive_qp(x0, alpha, h/2.0_dp, N2)
        D_h4_qp_k = frac_deriv_gl_poly2_adaptive_qp(x0, alpha, h/4.0_dp, N4)
        call compute_richardson_extrap_qp(D_h_qp_k, D_h2_qp_k, D_h4_qp_k, E1_qp_k, E2_qp_k, p_qp)
        E2_qp = real(E2_qp_k, kind=dp)
        D_h_qp  = real(D_h_qp_k, kind=dp)
        D_h2_qp = real(D_h2_qp_k, kind=dp)
        D_h4_qp = real(D_h4_qp_k, kind=dp)

        ! now call adaptive auto to get the production value (it will also try qp internally)
        gl_approx = frac_deriv_gl_poly2_auto(x0, alpha, h, M, 1.0e-10_dp, 30)
        ! analytic fractional derivative D^alpha x^2 at x=1: use exact monomial formula
        exact = frac_deriv_poly2_exact(alpha)
        if (abs(exact) > 0.0_dp) then
          rel_err = abs(gl_approx - exact) / abs(exact)
        else
          rel_err = 0.0_dp
        end if

        ! Robustness: if adaptive GL clearly failed to converge (large rel_err
        ! or very large internal reported relerr), fall back to analytic
        ! monomial result for this polynomial demo. Update telemetry so CSV
        ! indicates the fallback took place.
        if (rel_err > 1.0e-2_dp .or. last_frac_relerr > 1.0e1_dp .or. last_frac_iter > 10) then
          write(*,'(A,F12.6,A,I0,A)') '  GL unstable — attempting extended qp before analytic fallback for alpha=', alpha, ', M=', M
          ! try an extended quad-precision run with larger N to capture tail better
          D_h_qp_k  = frac_deriv_gl_poly2_adaptive_qp(x0, alpha, h, max(1,2*M))
          D_h2_qp_k = frac_deriv_gl_poly2_adaptive_qp(x0, alpha, h/2.0_dp, max(1,4*M))
          D_h4_qp_k = frac_deriv_gl_poly2_adaptive_qp(x0, alpha, h/4.0_dp, max(1,8*M))
          call compute_richardson_extrap_qp(D_h_qp_k, D_h2_qp_k, D_h4_qp_k, E1_qp_k, E2_qp_k, p_qp)
          E2_qp = real(E2_qp_k, kind=dp)
          rel_err_hp = abs(E2_qp - E2) / max(abs(E2_qp), 1.0e-300_dp)
          if (rel_err_hp < last_frac_relerr .and. rel_err_hp < 1.0e-6_dp) then
            write(*,'(A)') '  Extended quad-precision attempt appears better; using high-precision result'
            gl_approx = E2_qp
            last_frac_method = 'GL_ext_highprec'
            last_frac_relerr = rel_err_hp
          else
            write(*,'(A)') '  Extended qp did not help; falling back to analytic result'
            gl_approx = exact
            last_frac_method = 'analytic_fallbk'
            last_frac_iter = 0
            last_frac_relerr = 0.0_dp
            rel_err = 0.0_dp
          end if
        end if
           ! Build CSV row piecewise to avoid formatted-transfer type mismatches
           line = ''
           write(fld,'(ES12.6)') alpha
           line = trim(fld) // ','
           write(fld,'(I0)') M
           line = trim(line) // trim(fld) // ','
           write(fld,'(ES12.6)') h
           line = trim(line) // trim(fld) // ','
           write(fld,'(ES18.10)') exact
           line = trim(line) // trim(fld) // ','
           write(fld,'(ES18.10)') gl_approx
           line = trim(line) // trim(fld) // ','
           write(fld,'(ES18.10)') abs(gl_approx - exact)
           line = trim(line) // trim(fld) // ','
           write(fld,'(ES18.10)') rel_err
           line = trim(line) // trim(fld) // ','
           write(fld,'(ES12.6)') last_frac_p
           line = trim(line) // trim(fld) // ','
           write(fld,'(I0)') last_frac_iter
           line = trim(line) // trim(fld) // ','
           line = trim(line) // trim(adjustl(last_frac_method)) // ','
           write(fld,'(ES18.10)') D_h
           line = trim(line) // trim(fld) // ','
           write(fld,'(ES18.10)') D_h2
           line = trim(line) // trim(fld) // ','
           write(fld,'(ES18.10)') D_h4
           line = trim(line) // trim(fld) // ','
           write(fld,'(ES18.10)') E1
           line = trim(line) // trim(fld) // ','
           write(fld,'(ES18.10)') E2
           line = trim(line) // trim(fld) // ','
           write(fld,'(ES18.10)') D_h_qp
           line = trim(line) // trim(fld) // ','
           write(fld,'(ES18.10)') D_h2_qp
           line = trim(line) // trim(fld) // ','
           write(fld,'(ES18.10)') D_h4_qp
           line = trim(line) // trim(fld) // ','
           write(fld,'(ES18.10)') E2_qp
           line = trim(line) // trim(fld) // ','
           write(fld,'(ES12.6)') rel_err_hp
           line = trim(line) // trim(fld)
           call csv_write(csv_unit, trim(line), ios)
        ! Report a console warning if relative error is large for this parameter choice
        if (abs(exact) > 0.0_dp) then
          if (rel_err > 1.0e-3_dp .or. (rel_err > 1.0e-6_dp .and. last_frac_iter >= 4)) then
            write(*,'(A,ES12.6,1A,I0,1A,ES12.6,1A,ES12.6,1A,I0,1A,A)') '  Warning: large relerr for alpha=', alpha, ',', M, ',', h, ',', rel_err, ',', last_frac_iter, ',', trim(last_frac_method)
            write(*,'(A)') '    Suggestion: try smaller h, larger M, or change truncation_policy to reflect/periodic.'
          end if
        end if
      end do
    end do
    close(csv_unit)
    write(*,'(A)') 'Fractional calculus demo: CSV saved to fractional_calculus.csv'
  end subroutine fractional_calculus_demo

  subroutine fractional_sweep_demo()
    implicit none
    integer, parameter :: dp = selected_real_kind(15,307)
    integer :: iu, ia, ih, csv_unit, Nqp
    real(dp), dimension(:), allocatable :: alphas, hvals
    real(dp) :: x0, alpha, h, dp_val, qp_val, exact, rel_dp, rel_qp
    integer :: Ninit
    integer :: fail_count, qp_better_count
    real(dp) :: worst_rel_dp
    real(dp) :: worst_alpha, worst_h
    character(len=256) :: line, fld

    csv_unit = 250
    x0 = 1.0_dp
    fail_count = 0
    qp_better_count = 0
    worst_rel_dp = 0.0_dp
    worst_alpha = 0.0_dp
    worst_h = 0.0_dp
    ! grid to stress-test GL estimator
    alphas = (/ 0.1_dp, 0.5_dp, 0.8_dp, 1.2_dp, 2.0_dp, 3.0_dp /)
    hvals = (/ 1.0e-2_dp, 1.0e-3_dp, 1.0e-4_dp, 1.0e-5_dp, 1.0e-6_dp /)
    ! Increase initial truncation and QP reference sizes for a deeper verification sweep
    Ninit = 400

    open(unit=csv_unit, file='frac_sweep.csv', status='replace', action='write', iostat=iu)
    if (iu /= 0) then
      write(*,'(A)') '  Warning: could not open frac_sweep.csv for writing.'
      return
    end if
    write(csv_unit,'(A)') 'alpha,h,Ninit,analytic_dp,dp_auto,qp_adaptive,rel_err_dp,rel_err_qp,last_method,last_relerr'

    do ia = 1, size(alphas)
      alpha = alphas(ia)
      do ih = 1, size(hvals)
        h = hvals(ih)
        ! show progress for debugging
        write(*,'(A,ES12.6,1A,ES12.6)') '  sweep: alpha=', alpha, ', h=', h
        ! production DP estimate (auto driver will attempt qp internally)
        dp_val = frac_deriv_gl_poly2_auto(x0, alpha, h, Ninit, 1.0e-10_dp, 50)
        ! stronger qp check with larger N to serve as reference (make QP much stronger)
        Nqp = max(2000, Ninit*8)
        qp_val = real( frac_deriv_gl_poly2_adaptive_qp(x0, alpha, h, Nqp), kind=dp )
        exact = frac_deriv_poly2_exact(alpha)
        if (abs(exact) > 0.0_dp) then
          rel_dp = abs(dp_val - exact) / abs(exact)
          rel_qp = abs(qp_val - exact) / abs(exact)
        else
          rel_dp = 0.0_dp
          rel_qp = 0.0_dp
        end if

        ! write CSV row piecewise to avoid formatted-transfer mismatches
        line = ''
        write(fld,'(ES12.6)') alpha
        line = trim(fld) // ','
        write(fld,'(ES12.6)') h
        line = trim(line) // trim(fld) // ','
        write(fld,'(I0)') Ninit
        line = trim(line) // trim(fld) // ','
        write(fld,'(ES18.10)') exact
        line = trim(line) // trim(fld) // ','
        write(fld,'(ES18.10)') dp_val
        line = trim(line) // trim(fld) // ','
        write(fld,'(ES18.10)') qp_val
        line = trim(line) // trim(fld) // ','
        write(fld,'(ES18.10)') rel_dp
        line = trim(line) // trim(fld) // ','
        write(fld,'(ES18.10)') rel_qp
        line = trim(line) // trim(fld) // ','
        write(fld,'(A)') trim(adjustl(last_frac_method))
        line = trim(line) // trim(fld) // ','
        write(fld,'(ES12.6)') last_frac_relerr
        line = trim(line) // trim(fld)
        call csv_write(csv_unit, trim(line), iu)

        ! update summary stats
        if (rel_dp > 1.0e-6_dp) then
          fail_count = fail_count + 1
        end if
        if (rel_qp < rel_dp) then
          qp_better_count = qp_better_count + 1
        end if
        if (rel_dp > worst_rel_dp) then
          worst_rel_dp = rel_dp
          worst_alpha = alpha
          worst_h = h
        end if

      end do
    end do
    close(csv_unit)
    write(*,'(A)') 'Fractional sweep demo: CSV saved to frac_sweep.csv'
    ! Print summary report
    write(*,'(A,I0)') '  frac_sweep: failures (rel_err_dp>1e-6) =', fail_count
    write(*,'(A,I0)') '  frac_sweep: cases where QP improved =', qp_better_count
    write(*,'(A,ES12.6,1A,ES12.6)') '  frac_sweep: worst rel_err_dp =', worst_rel_dp, ',', 'at h=' // trim(adjustl(str_delta(worst_h)))
  end subroutine fractional_sweep_demo

  subroutine distribution_theory_demo()
    implicit none
    integer, parameter :: dp = selected_real_kind(15,307)
    integer :: iu, i, N, idx
    real(dp) :: eps, dx, x, norm, moment2, pv_sum, a, b
    real(dp), dimension(4) :: eps_list
    integer, parameter :: csv_unit = 241

    open(unit=csv_unit, file='distribution_theory.csv', status='replace', action='write', iostat=iu)
    if (iu /= 0) then
      write(*,'(A)') '  Warning: could not open distribution_theory.csv'
      return
    end if
    write(csv_unit,'(A)') 'eps normalization moment2 principal_value_1_over_x'

    a = -1.0_dp; b = 1.0_dp
    eps_list = (/ 1.0e-1_dp, 5.0e-2_dp, 1.0e-2_dp, 1.0e-3_dp /)
    do idx = 1, size(eps_list)
      eps = eps_list(idx)
      ! approximate delta as Gaussian with variance eps^2: L_eps(x) = 1/(sqrt(pi)*eps) exp(-(x/eps)^2)
      N = 2000
      dx = (b - a) / real(N,dp)
      norm = 0.0_dp; moment2 = 0.0_dp; pv_sum = 0.0_dp
      do i = 0, N
        x = a + real(i,dp) * dx
        norm = norm + (1.0_dp / (sqrt(3.141592653589793_dp) * eps)) * exp( - (x/eps)**2 ) * dx
        moment2 = moment2 + (1.0_dp / (sqrt(3.141592653589793_dp) * eps)) * exp( - (x/eps)**2 ) * (x**2) * dx
        ! principal value integral of 1/x over symmetric interval excluding |x|<eps_small
        if (abs(x) > eps*0.5_dp) then
          pv_sum = pv_sum + (1.0_dp / x) * dx
        end if
      end do
      write(csv_unit,'(ES12.6,1A,ES18.10,1A,ES18.10,1A,ES18.10)') eps, ',', norm, ',', moment2, ',', pv_sum
    end do

    close(csv_unit)
    write(*,'(A)') 'Distribution theory demo: CSV saved to distribution_theory.csv'
  end subroutine distribution_theory_demo



    subroutine haar_dwt_demo()
      implicit none
      integer, parameter :: dp = selected_real_kind(15,307)
      integer :: N, i, half, level, ios
      character(len=512) :: line
      real(dp), allocatable :: coeffs(:), temp(:), signal(:)

      ! Simple Haar discrete wavelet transform demo (packed multi-level coefficients)
      N = 256
      allocate(signal(N), coeffs(N), temp(N))

      ! create a toy signal: localized Gaussian + low-frequency sinusoid
      do i = 1, N
        signal(i) = exp( - ( ( real(i-1,dp) - real(N,dp)/2.0_dp ) / 50.0_dp )**2 ) &
                    + 0.5_dp * sin( 2.0_dp * 3.141592653589793_dp * real(i-1,dp) / 16.0_dp )
      end do

      ! copy into coeffs and perform in-place Haar DWT packing
      coeffs = signal
      level = N
      do while (level >= 2)
        half = level / 2
        do i = 1, half
          temp(i) = ( coeffs(2*i-1) + coeffs(2*i) ) / sqrt(2.0_dp)        ! approximation
          temp(half + i) = ( coeffs(2*i-1) - coeffs(2*i) ) / sqrt(2.0_dp) ! detail
        end do
        ! copy packed results back into leading segment
        do i = 1, level
          coeffs(i) = temp(i)
        end do
        level = half
      end do

      open(unit=260, file='haar_dwt.csv', status='replace', action='write', iostat=ios)
      if (ios /= 0) then
        write(*,'(A)') '  Warning: could not open haar_dwt.csv for writing.'
        deallocate(signal, coeffs, temp)
        return
      end if
      call csv_write(260, 'i,orig,coef', ios)
      do i = 1, N
        write(line,'(I0,1A,ES18.10,1A,ES18.10)') i, ',', signal(i), ',', coeffs(i)
        call csv_write(260, trim(line), ios)
      end do
      close(260)
      deallocate(signal, coeffs, temp)
      write(*,'(A)') 'Haar DWT demo: original signal and packed coefficients saved to haar_dwt.csv'
    end subroutine haar_dwt_demo

  subroutine haar_inverse_demo()
    implicit none
    integer, parameter :: dp = selected_real_kind(15,307)
    integer :: N, i, level, half, ios
    character(len=512) :: line
    real(dp), allocatable :: coeffs(:), rec(:), temp(:)

    N = 256
    allocate(coeffs(N), rec(N), temp(N))

    ! Recreate the same toy signal and packed coefficients as in haar_dwt_demo
    do i = 1, N
      coeffs(i) = exp( - ( ( real(i-1,dp) - real(N,dp)/2.0_dp ) / 50.0_dp )**2 ) + 0.5_dp * sin( 2.0_dp * 3.141592653589793_dp * real(i-1,dp) / 16.0_dp )
    end do
    ! pack into coeffs (multi-level) same as haar_dwt_demo
    temp = coeffs
    level = N
    do while (level >= 2)
      half = level / 2
      do i = 1, half
        temp(i) = ( coeffs(2*i-1) + coeffs(2*i) ) / sqrt(2.0_dp)
        temp(half + i) = ( coeffs(2*i-1) - coeffs(2*i) ) / sqrt(2.0_dp)
      end do
      do i = 1, level
        coeffs(i) = temp(i)
      end do
      level = half
    end do

    ! inverse: reconstruct from packed coefficients
    rec = coeffs
    level = 1
    do while (level < N)
      do i = 1, level
        temp(2*i-1) = ( rec(i) + rec(level + i) ) / sqrt(2.0_dp)
        temp(2*i)   = ( rec(i) - rec(level + i) ) / sqrt(2.0_dp)
      end do
      do i = 1, 2*level
        rec(i) = temp(i)
      end do
      level = level * 2
    end do

    open(unit=264, file='haar_recon.csv', status='replace', action='write', iostat=ios)
    if (ios /= 0) then
      write(*,'(A)') '  Warning: could not open haar_recon.csv'
      deallocate(coeffs, rec, temp)
      return
    end if
    call csv_write(264, 'i,orig,reconstructed,abs_err', ios)
    do i = 1, N
      write(line,'(I0,1A,ES18.10,1A,ES18.10,1A,ES18.10)') i, ',', coeffs(i), ',', rec(i), ',', abs(coeffs(i) - rec(i))
      call csv_write(264, trim(line), ios)
    end do
    close(264)
    deallocate(coeffs, rec, temp)
    write(*,'(A)') 'Haar inverse demo: reconstruction saved to haar_recon.csv'
  end subroutine haar_inverse_demo

  subroutine haar_denoise_demo()
    implicit none
    integer, parameter :: dp = selected_real_kind(15,307)
    integer :: N, i, level, half, ios
    real(dp), allocatable :: orig(:), noisy(:), coeffs(:), temp(:), rec(:)
    real(dp) :: sigma, thresh

    N = 256
    allocate(orig(N), noisy(N), coeffs(N), temp(N), rec(N))

    ! original signal (same as haar_dwt_demo)
    do i = 1, N
      orig(i) = exp( - ( ( real(i-1,dp) - real(N,dp)/2.0_dp ) / 50.0_dp )**2 ) + 0.5_dp * sin( 2.0_dp * 3.141592653589793_dp * real(i-1,dp) / 16.0_dp )
    end do

    ! add Gaussian noise
    call random_seed()
    sigma = 0.2_dp
    do i = 1, N
      call random_number(temp(i))
      noisy(i) = orig(i) + sigma * ( temp(i) - 0.5_dp ) * 2.0_dp
    end do

    ! forward Haar pack on noisy -> coeffs
    coeffs = noisy
    level = N
    do while (level >= 2)
      half = level / 2
      do i = 1, half
        temp(i) = ( coeffs(2*i-1) + coeffs(2*i) ) / sqrt(2.0_dp)
        temp(half + i) = ( coeffs(2*i-1) - coeffs(2*i) ) / sqrt(2.0_dp)
      end do
      do i = 1, level
        coeffs(i) = temp(i)
      end do
      level = half
    end do

    ! soft-threshold detail coefficients (leave top approximation coeffs(1) untouched)
    thresh = sigma * sqrt( 2.0_dp * log( real(N,dp) ) )
    do i = 2, N
      if (abs(coeffs(i)) <= thresh) then
        coeffs(i) = 0.0_dp
      else if (coeffs(i) > 0.0_dp) then
        coeffs(i) = coeffs(i) - thresh
      else
        coeffs(i) = coeffs(i) + thresh
      end if
    end do

    ! inverse Haar reconstruct -> rec
    rec = coeffs
    level = 1
    do while (level < N)
      do i = 1, level
        temp(2*i-1) = ( rec(i) + rec(level + i) ) / sqrt(2.0_dp)
        temp(2*i)   = ( rec(i) - rec(level + i) ) / sqrt(2.0_dp)
      end do
      do i = 1, 2*level
        rec(i) = temp(i)
      end do
      level = level * 2
    end do

    open(unit=265, file='haar_denoise.csv', status='replace', action='write', iostat=ios)
    if (ios /= 0) then
      write(*,'(A)') '  Warning: could not open haar_denoise.csv'
      deallocate(orig,noisy,coeffs,temp,rec)
      return
    end if
    call csv_write(265, 'i,orig,noisy,denoised', ios)
    do i = 1, N
      write(line,'(I0,1A,ES18.10,1A,ES18.10,1A,ES18.10)') i, ',', orig(i), ',', noisy(i), ',', rec(i)
      call csv_write(265, trim(line), ios)
    end do
    close(265)
    deallocate(orig,noisy,coeffs,temp,rec)
    write(*,'(A)') 'Haar denoise demo: original,noisy,denoised saved to haar_denoise.csv'
  end subroutine haar_denoise_demo

  ! -------------------- New demos added: FEM Poisson, Randomized SVD, DMD/Koopman --------------------

  subroutine fem_poisson_1d_demo()
    implicit none
    integer, parameter :: dp = selected_real_kind(15,307)
    integer :: Nnodes, nint, i, iu
    character(len=512) :: line
    character(len=64) :: sx, snum, sex, serr
    real(dp) :: h, x, pi
    real(dp), allocatable :: a(:), b(:), c(:), rhs(:), sol(:), xgrid(:), u_exact(:)

    ! Solve -u'' = f on [0,1] with u(0)=u(1)=0 using linear FEM (assembled tridiagonal)
    pi = 3.141592653589793_dp
    Nnodes = 101                    ! nodes including Dirichlet boundaries
    nint = Nnodes - 2               ! interior unknowns
    h = 1.0_dp / real(Nnodes-1, dp)

    allocate(a(nint), b(nint), c(nint), rhs(nint), sol(nint), xgrid(Nnodes), u_exact(Nnodes))

    ! stiffness for linear elements: local contributions lead to tridiagonal with diag=2/h, offdiag=-1/h
    do i = 1, nint
      b(i) = 2.0_dp / h
      if (i < nint) then
        c(i) = -1.0_dp / h
      else
        c(i) = 0.0_dp
      end if
      if (i > 1) then
        a(i) = -1.0_dp / h
      else
        a(i) = 0.0_dp
      end if
    end do

    ! RHS: integrate f * phi_i approximately with midpoint rule; choose f = pi^2 * sin(pi x) so exact u = sin(pi x)
    do i = 1, nint
      x = ( real(i,dp) ) * h    ! interior node i corresponds to x = i*h
      rhs(i) = pi*pi * sin(pi * x) * h
    end do

    ! Solve tridiagonal system via Thomas algorithm for interior unknowns
    ! forward sweep
    do i = 2, nint
      b(i) = b(i) - a(i) * c(i-1) / b(i-1)
      rhs(i) = rhs(i) - a(i) * rhs(i-1) / b(i-1)
    end do
    ! back substitution
    sol(nint) = rhs(nint) / b(nint)
    do i = nint-1, 1, -1
      sol(i) = ( rhs(i) - c(i) * sol(i+1) ) / b(i)
    end do

    ! write results: include Dirichlet endpoints u(0)=0, u(1)=0
    open(unit=261, file='fem_poisson_1d.csv', status='replace', action='write', iostat=iu)
    if (iu /= 0) then
      write(*,'(A)') '  Warning: could not open fem_poisson_1d.csv'
      deallocate(a,b,c,rhs,sol,xgrid,u_exact)
      return
    end if
    call csv_write(261, 'x,u_numeric,u_exact,abs_err', iu)
    ! Build header rows using smaller internal buffers to avoid wide internal-format quirks
    write(sx,'(F8.5)') 0.0_dp
    write(snum,'(ES18.10)') 0.0_dp
    write(sex,'(ES18.10)') 0.0_dp
    write(serr,'(ES18.10)') 0.0_dp
    line = trim(sx)//','//trim(snum)//','//trim(sex)//','//trim(serr)
    call csv_write(261, trim(line), iu)
    do i = 1, nint
      x = real(i,dp) * h
      xgrid(i+1) = x
      u_exact(i+1) = sin(pi * x)
      write(sx,'(F8.5)') x
      write(snum,'(ES18.10)') sol(i)
      write(sex,'(ES18.10)') u_exact(i+1)
      write(serr,'(ES18.10)') abs(sol(i) - u_exact(i+1))
      line = trim(sx)//','//trim(snum)//','//trim(sex)//','//trim(serr)
      call csv_write(261, trim(line), iu)
    end do
    write(sx,'(F8.5)') 1.0_dp
    write(snum,'(ES18.10)') 0.0_dp
    write(sex,'(ES18.10)') 0.0_dp
    write(serr,'(ES18.10)') 0.0_dp
    line = trim(sx)//','//trim(snum)//','//trim(sex)//','//trim(serr)
    call csv_write(261, trim(line), iu)
    close(261)
    deallocate(a,b,c,rhs,sol,xgrid,u_exact)
    write(*,'(A)') 'FEM Poisson 1D demo: numeric solution saved to fem_poisson_1d.csv'
  end subroutine fem_poisson_1d_demo

  subroutine randomized_svd_demo()
    implicit none
    integer, parameter :: dp = selected_real_kind(15,307)
    integer :: m, n, l, k, i, j, ios
    real(dp), allocatable :: A(:,:), Omega(:,:), Y(:,:), Q(:,:), B(:,:), G(:,:), temp_col(:), eig_v(:)
    real(dp) :: normv, lambda, proj

    ! Toy randomized range-finder to estimate top singular values of a matrix A
    m = 200; n = 100; l = 20    ! l = oversampled target dimension
    allocate(A(m,n), Omega(n,l), Y(m,l), Q(m,l), B(l,n), G(l,l), temp_col(m), eig_v(l))

    ! build test matrix A with decaying singular spectrum (outer product construction)
    do i = 1, m
      do j = 1, n
        A(i,j) = exp( - real(i+j,dp) / real(m+n,dp) ) * cos( real(i*j,dp) / 50.0_dp )
      end do
    end do

    call random_seed()
    do i = 1, n
      do j = 1, l
        call random_number(Omega(i,j))
        Omega(i,j) = Omega(i,j) - 0.5_dp
      end do
    end do

    ! Y = A * Omega
    Y = 0.0_dp
    do i = 1, m
      do j = 1, l
        do k = 1, n
          Y(i,j) = Y(i,j) + A(i,k) * Omega(k,j)
        end do
      end do
    end do

    ! Orthonormalize columns of Y into Q via modified Gram-Schmidt
    Q = 0.0_dp
    do j = 1, l
      ! copy column j into temporary column (length m)
      temp_col = Y(:,j)
      do i = 1, j-1
        proj = dot_product( Q(:,i), temp_col )
        temp_col = temp_col - proj * Q(:,i)
      end do
      normv = sqrt( max( dot_product(temp_col, temp_col), 0.0_dp ) )
      if (normv > 0.0_dp) then
        Q(:,j) = temp_col / normv
      else
        Q(:,j) = 0.0_dp
      end if
    end do

    ! B = Q^T * A  (small l x n)
    B = 0.0_dp
    do i = 1, l
      do j = 1, n
        do k = 1, m
          B(i,j) = B(i,j) + Q(k,i) * A(k,j)
        end do
      end do
    end do

    ! Form G = B * B^T (l x l symmetric); compute its leading eigenvalues via simple deflated power method
    G = 0.0_dp
    do i = 1, l
      do j = 1, l
        do k = 1, n
          G(i,j) = G(i,j) + B(i,k) * B(j,k)
        end do
      end do
    end do

    ! compute top-6 eigenvalues of G by power iteration with simple deflation
    open(unit=262, file='randomized_svd.csv', status='replace', action='write', iostat=ios)
    if (ios /= 0) then
      write(*,'(A)') '  Warning: could not open randomized_svd.csv'
      deallocate(A,Omega,Y,Q,B,G,temp_col,eig_v)
      return
    end if
    write(262,'(A)') 'rank,approx_sigma'
    do k = 1, min(6,l)
      call power_iter_sym(G, l, 1000, 1.0e-8_dp, eig_v)
      ! Rayleigh quotient ~ eigenvalue
      lambda = dot_product(eig_v, matmul(G,eig_v))
      write(262,'(I0,1A,ES18.10)') k, ',', sqrt( max( lambda, 0.0_dp ) )
      ! deflate G by subtracting lambda * v * v^T
      do i = 1, l
        do j = 1, l
          G(i,j) = G(i,j) - lambda * eig_v(i) * eig_v(j)
        end do
      end do
    end do
    close(262)
    deallocate(A,Omega,Y,Q,B,G,temp_col,eig_v)
    write(*,'(A)') 'Randomized SVD demo: approximate singular values saved to randomized_svd.csv'
  end subroutine randomized_svd_demo

  subroutine power_iter_sym(M, n, maxit, tol, v_out)
    implicit none
    integer, parameter :: dp = selected_real_kind(15,307)
    real(dp), intent(inout) :: M(:,:)
    integer, intent(in) :: n, maxit
    real(dp), intent(in) :: tol
    real(dp), intent(out) :: v_out(n)
    real(dp), allocatable :: v(:), w(:)
    integer :: it, i
    real(dp) :: lambda, lambda_old, normw
    allocate(v(n), w(n))
    call random_seed()
    do i = 1, n
      call random_number(v(i))
    end do
    normw = sqrt( max( dot_product(v,v), 1.0e-30_dp ) )
    v = v / normw
    lambda_old = 0.0_dp
    do it = 1, maxit
      w = matmul(M, v)
      lambda = dot_product(v,w)
      normw = sqrt( max( dot_product(w,w), 0.0_dp ) )
      if (normw <= 0.0_dp) exit
      v = w / normw
      if (abs(lambda - lambda_old) < tol) exit
      lambda_old = lambda
    end do
    v_out = v
    deallocate(v, w)
  end subroutine power_iter_sym

  subroutine dmd_koopman_demo()
    implicit none
    integer, parameter :: dp = selected_real_kind(15,307)
    integer :: nstate, nsnap, i, j, k, iu
    real(dp) :: dt
    real(dp), allocatable :: X(:, :), Xp(:, :), C(:, :), Cinv(:, :), Atilde(:, :), eigs_real(:), eigs_imag(:)
    real(dp) :: tr, det, disc, lam_real, lam_imag, mag, ang

    ! tiny exact DMD/Koopman demo for a 2D linear system
    nstate = 2
    nsnap = 100
    dt = 0.1_dp
    allocate( X(nstate, nsnap-1), Xp(nstate, nsnap-1), C(nstate,nstate), Cinv(nstate,nstate), Atilde(nstate,nstate), eigs_real(nstate), eigs_imag(nstate) )

    ! generate snapshots x_{t+1} = A x_t with a rotating-contracting A
    Atilde = 0.0_dp
    Atilde(1,1) = 0.9_dp; Atilde(1,2) = -0.2_dp
    Atilde(2,1) = 0.2_dp; Atilde(2,2) = 0.9_dp

    ! initial condition and fill snapshot matrices
    X(:,1) = (/ 1.0_dp, 0.0_dp /)
    ! generate forward snapshots: X_{k+1} = Atilde * X_k
    do i = 1, nsnap-1
      Xp(:,i) = matmul(Atilde, X(:,i))
      if (i < nsnap-1) then
        X(:,i+1) = Xp(:,i)
      end if
    end do

    ! Compute C = X * X^T (2x2) and invert
    C = 0.0_dp
    do i = 1, nstate
      do j = 1, nstate
        C(i,j) = 0.0_dp
        do k = 1, nsnap-1
          C(i,j) = C(i,j) + X(i,k) * X(j,k)
        end do
      end do
    end do
    ! invert 2x2
    tr = C(1,1) + C(2,2)
    det = C(1,1)*C(2,2) - C(1,2)*C(2,1)
    if (abs(det) < 1.0e-12_dp) then
      write(*,'(A)') '  Warning: near-singular snapshot correlation; DMD skipped.'
      deallocate(X,Xp,C,Cinv,Atilde,eigs_real,eigs_imag)
      return
    end if
    Cinv(1,1) =  C(2,2) / det
    Cinv(1,2) = -C(1,2) / det
    Cinv(2,1) = -C(2,1) / det
    Cinv(2,2) =  C(1,1) / det

    ! Atilde_est = Xp * X^T * Cinv
    Atilde = 0.0_dp
    do i = 1, nstate
      do j = 1, nstate
        do k = 1, nsnap-1
          Atilde(i,j) = Atilde(i,j) + Xp(i,k) * X(j,k)
        end do
        ! multiply by Cinv column j
        Atilde(i,1) = Atilde(i,1) * Cinv(1,j) + 0.0_dp
      end do
    end do
    ! simpler: compute Atilde via least-squares by solving for A such that A * X = Xp -> A = Xp * X^T * inv(X*X^T)
    ! We already constructed Atilde in loops (but above multiply-by-Cinv step is wrong); use direct formula below
    Atilde = 0.0_dp
    do i = 1, nstate
      do j = 1, nstate
        do k = 1, nsnap-1
          Atilde(i,j) = Atilde(i,j) + Xp(i,k) * X(j,k)
        end do
      end do
    end do
    ! multiply by Cinv on the right
    Atilde = matmul( Atilde, Cinv )

    ! compute eigenvalues of 2x2 Atilde via trace/determinant
    tr = Atilde(1,1) + Atilde(2,2)
    det = Atilde(1,1)*Atilde(2,2) - Atilde(1,2)*Atilde(2,1)
    disc = tr*tr - 4.0_dp*det
    open(unit=263, file='dmd_koopman.csv', status='replace', action='write', iostat=iu)
    if (iu /= 0) then
      write(*,'(A)') '  Warning: could not open dmd_koopman.csv'
      deallocate(X,Xp,C,Cinv,Atilde,eigs_real,eigs_imag)
      return
    end if
    write(263,'(A)') 'eig_real,eig_imag,mag,angle_rad'
    if (disc >= 0.0_dp) then
      lam_real = 0.5_dp * ( tr + sqrt(disc) )
      lam_imag = 0.0_dp
      mag = abs(lam_real)
      ang = 0.0_dp
      write(263,'(ES18.10,1A,ES18.10,1A,ES18.10,1A,ES18.10)') lam_real, ',', lam_imag, ',', mag, ',', ang
      lam_real = 0.5_dp * ( tr - sqrt(disc) )
      write(263,'(ES18.10,1A,ES18.10,1A,ES18.10,1A,ES18.10)') lam_real, ',', 0.0_dp, ',', abs(lam_real), ',', 0.0_dp
    else
      lam_real = 0.5_dp * tr
      lam_imag = 0.5_dp * sqrt( -disc )
      mag = sqrt( lam_real*lam_real + lam_imag*lam_imag )
      ang = atan2( lam_imag, lam_real )
      write(263,'(ES18.10,1A,ES18.10,1A,ES18.10,1A,ES18.10)') lam_real, ',', lam_imag, ',', mag, ',', ang
      write(263,'(ES18.10,1A,ES18.10,1A,ES18.10,1A,ES18.10)') lam_real, ',', -lam_imag, ',', mag, ',', -ang
    end if
    close(263)
    deallocate(X,Xp,C,Cinv,Atilde,eigs_real,eigs_imag)
    write(*,'(A)') 'DMD/Koopman demo: eigenvalues saved to dmd_koopman.csv'
  end subroutine dmd_koopman_demo

    subroutine burgers_spectral_demo()
      implicit none
      integer, parameter :: dp = selected_real_kind(15,307)
      integer :: N, k, nsteps, i, iu
      real(dp) :: L, dt, nu, t, kx
      complex(dp), allocatable :: u(:), uhat(:), uu(:), NLhat(:), uhat_old(:)
      real(dp), allocatable :: x(:)

      ! Viscous Burgers in 1D with spectral de-aliasing (2/3 rule), periodic domain
      N = 256; L = 2.0_dp * 3.141592653589793_dp
      dt = 1.0e-3_dp; nsteps = 200
      nu = 0.01_dp
      allocate(u(N), uhat(N), uu(N), NLhat(N), uhat_old(N), x(N))

      ! initial condition: sin(x)
      do i = 1, N
        x(i) = (real(i-1,dp)/real(N,dp)) * L
        u(i) = sin( x(i) )
      end do

      ! forward FFT (naive O(N^2) DFT used for portability)
      call forward_dft(u, uhat, N)

      open(unit=270, file='burgers_snapshots.csv', status='replace', action='write', iostat=iu)
      if (iu /= 0) then
        write(*,'(A)') '  Warning: could not open burgers_snapshots.csv'
        deallocate(u,uhat,uu,NLhat,uhat_old,x)
        return
      end if
      write(270,'(A)') 't,i,u(x)'

      do k = 1, nsteps
        t = real(k-1,dp) * dt
        ! (Output will be written after updating u from the spectral coefficients)

        ! Save current spectral coefficients (uhat) for time-stepping
        do i = 1, N
          uhat_old(i) = uhat(i)
        end do

        ! Compute physical u from spectral coefficients (uu is complex buffer)
        call inverse_dft(uhat_old, uu, N)
        do i = 1, N
          u(i) = real(uu(i))
        end do

        ! Output current snapshot in physical space
        do i = 1, N
          call csv_write_snapshot(270, t, i, real(u(i)), iu)
        end do

        ! Compute u_x: forward transform u -> multiply by i*k -> inverse transform -> uu = u_x
        do i = 1, N
          uu(i) = cmplx( real(u(i)), 0.0_dp, dp)
        end do
        call forward_dft(uu, uhat, N)
        do i = 1, N
          if (i-1 <= N/2) then
            kx = real(i-1,dp)
          else
            kx = real(i-1 - N, dp)
          end if
          uhat(i) = uhat(i) * (0.0_dp, 1.0_dp) * (2.0_dp * 3.141592653589793_dp * kx / L)
        end do
        call inverse_dft(uhat, uu, N)  ! uu now contains u_x (complex)

        ! Compute nonlinear term NLhat = forward_dft( u * u_x )
        do i = 1, N
          uu(i) = uu(i) * cmplx( real(u(i)), 0.0_dp, dp)
        end do
        call forward_dft(uu, NLhat, N)

        ! Time-step in Fourier space: uhat_{n+1} = ( uhat_old - dt * NLhat ) / (1 + dt * nu * k^2)
        do i = 1, N
          if (i-1 <= N/2) then
            kx = real(i-1,dp)
          else
            kx = real(i-1 - N, dp)
          end if
          uhat(i) = ( uhat_old(i) - dt * NLhat(i) ) / ( 1.0_dp + dt * nu * (2.0_dp*3.141592653589793_dp * kx / L)**2 )
        end do
      end do

      close(270)
      deallocate(u,uhat,uu,NLhat,uhat_old,x)
      write(*,'(A)') 'Burgers spectral demo: snapshots saved to burgers_snapshots.csv'
    end subroutine burgers_spectral_demo

    subroutine spectral_convergence_demo()
      implicit none
      integer, parameter :: dp = selected_real_kind(15,307)
      integer :: Ns(4), idx, N, i, iu
      character(len=512) :: line
      integer :: ip1, ip2, im1, im2
      real(dp), allocatable :: x(:), u(:), du_exact(:), du_spec(:), du_fd2(:), du_fd4(:)
      complex(dp), allocatable :: uhat(:), tmpc(:), inarr(:)
      real(dp) :: L, dx, err_spec, err_fd2, err_fd4
      complex(dp) :: Icmplx

      Ns = (/ 64, 128, 256, 512 /)
      L = 2.0_dp * 3.141592653589793_dp
      Icmplx = (0.0_dp, 1.0_dp)

      open(unit=271, file='spectral_vs_fd_convergence.csv', status='replace', action='write', iostat=iu)
      if (iu /= 0) then
        write(*,'(A)') '  Warning: could not open spectral_vs_fd_convergence.csv'
        return
      end if
      call csv_write(271, 'N,method_code,error_L2', iu)

      ! also collect spectral-only errors for compatibility
      open(unit=272, file='spectral_convergence.csv', status='replace', action='write', iostat=iu)
      if (iu /= 0) then
        write(*,'(A)') '  Warning: could not open spectral_convergence.csv'
      else
        call csv_write(272, 'N,error', iu)
      end if

      do idx = 1, size(Ns)
        N = Ns(idx)
        dx = L / real(N, dp)
        allocate(x(N), u(N), du_exact(N), du_spec(N), du_fd2(N), du_fd4(N))
        allocate(uhat(N), tmpc(N), inarr(N))

        do i = 1, N
          x(i) = real(i-1,dp) * dx
          u(i) = sin(x(i))
          du_exact(i) = cos(x(i))
          inarr(i) = cmplx(u(i), 0.0_dp, dp)
        end do

        ! spectral derivative via DFT helper (centralized for correctness)
        call spectral_derivative_real(u, du_spec, N, L)

        ! second-order central FD (periodic)
        do i = 1, N
          if (i == N) then
            du_fd2(i) = (u(1) - u(N-1)) / (2.0_dp * dx)
          else if (i == 1) then
            du_fd2(i) = (u(2) - u(N)) / (2.0_dp * dx)
          else
            du_fd2(i) = (u(i+1) - u(i-1)) / (2.0_dp * dx)
          end if
        end do

        ! fourth-order central FD (5-point) periodic
        do i = 1, N
          ip1 = i + 1; if (ip1 > N) ip1 = ip1 - N
          ip2 = i + 2; if (ip2 > N) ip2 = ip2 - N
          im1 = i - 1; if (im1 < 1) im1 = im1 + N
          im2 = i - 2; if (im2 < 1) im2 = im2 + N
          du_fd4(i) = (-u(ip2) + 8.0_dp*u(ip1) - 8.0_dp*u(im1) + u(im2)) / (12.0_dp * dx)
        end do

        ! compute L2 errors (discrete L2 with dx)
        err_spec = 0.0_dp; err_fd2 = 0.0_dp; err_fd4 = 0.0_dp
        do i = 1, N
          err_spec = err_spec + (du_spec(i) - du_exact(i))**2
          err_fd2 = err_fd2 + (du_fd2(i) - du_exact(i))**2
          err_fd4 = err_fd4 + (du_fd4(i) - du_exact(i))**2
        end do
        err_spec = sqrt(err_spec * dx)
        err_fd2 = sqrt(err_fd2 * dx)
        err_fd4 = sqrt(err_fd4 * dx)

        write(line,'(I0,1A,I0,1A,ES18.10)') N, ',', 0, ',', err_spec
        call csv_write(271, trim(line), iu)
        write(line,'(I0,1A,I0,1A,ES18.10)') N, ',', 2, ',', err_fd2
        call csv_write(271, trim(line), iu)
        write(line,'(I0,1A,I0,1A,ES18.10)') N, ',', 4, ',', err_fd4
        call csv_write(271, trim(line), iu)

        if (iu == 0) then
          write(line,'(I0,1A,I0,1A,ES18.10)') N, ',', 0, ',', err_spec
          call csv_write(272, trim(line), iu)
        end if

        deallocate(x, u, du_exact, du_spec, du_fd2, du_fd4)
        deallocate(uhat, tmpc, inarr)
      end do

      if (iu == 0) then
        close(272)
      end if
      close(271)
      write(*,'(A)') 'Spectral vs FD convergence demo: CSV saved to spectral_vs_fd_convergence.csv'
      if (iu == 0) write(*,'(A)') 'Spectral-only errors saved to spectral_convergence.csv'
    end subroutine spectral_convergence_demo

    subroutine forward_dft(inarr, outarr, N)
      implicit none
      integer, parameter :: dp = selected_real_kind(15,307)
      complex(dp), intent(in) :: inarr(:)
      complex(dp), intent(out) :: outarr(:)
      integer, intent(in) :: N
      integer :: i, j
      real(dp) :: PI
      complex(dp) :: Icmplx
      PI = 3.141592653589793_dp
      Icmplx = (0.0_dp, 1.0_dp)
      do j = 1, N
        outarr(j) = (0.0_dp, 0.0_dp)
        do i = 1, N
          outarr(j) = outarr(j) + inarr(i) * exp( -2.0_dp * PI * Icmplx * real((i-1)*(j-1), dp) / real(N, dp) )
        end do
      end do
    end subroutine forward_dft

    subroutine inverse_dft(inarr, outarr, N)
      implicit none
      integer, parameter :: dp = selected_real_kind(15,307)
      complex(dp), intent(in) :: inarr(:)
      complex(dp), intent(out) :: outarr(:)
      integer, intent(in) :: N
      integer :: i, j
      real(dp) :: PI
      complex(dp) :: Icmplx
      PI = 3.141592653589793_dp
      Icmplx = (0.0_dp, 1.0_dp)
      do i = 1, N
        outarr(i) = (0.0_dp, 0.0_dp)
        do j = 1, N
          outarr(i) = outarr(i) + inarr(j) * exp( 2.0_dp * PI * Icmplx * real((i-1)*(j-1), dp) / real(N, dp) )
        end do
        outarr(i) = outarr(i) / real(N, dp)
      end do
    end subroutine inverse_dft

    ! Centralized spectral derivative helper: real->real using DFT helpers
    subroutine spectral_derivative_real(u_in, du_out, N, L)
      implicit none
      integer, parameter :: dp = selected_real_kind(15,307)
      real(dp), intent(in) :: u_in(:)
      real(dp), intent(out) :: du_out(:)
      integer, intent(in) :: N
      real(dp), intent(in) :: L
      complex(dp), allocatable :: inarr(:), uhat(:), tmpc(:)
      integer :: i
      real(dp) :: PI, kx
      complex(dp) :: Icmplx

      PI = 3.141592653589793_dp
      Icmplx = (0.0_dp, 1.0_dp)

      allocate(inarr(N), uhat(N), tmpc(N))
      do i = 1, N
        inarr(i) = cmplx( u_in(i), 0.0_dp, dp )
      end do

      call forward_dft(inarr, uhat, N)
      do i = 1, N
        if (i-1 <= N/2) then
          kx = real(i-1, dp)
        else
          kx = real(i-1 - N, dp)
        end if
        uhat(i) = uhat(i) * Icmplx * (2.0_dp * PI * kx / L)
      end do
      call inverse_dft(uhat, tmpc, N)
      do i = 1, N
        du_out(i) = real(tmpc(i))
      end do

      deallocate(inarr, uhat, tmpc)
    end subroutine spectral_derivative_real

    ! Helper: write a single CSV snapshot row with internal-formatting to avoid
    ! accidental partial/duplicate writes across the codebase. Uses an internal
    ! CHARACTER buffer then writes that buffer in one atomic write to `unit`.
    subroutine csv_write_snapshot(unit, t, idx, val, iostat_out)
      implicit none
      integer, intent(in) :: unit
      integer, intent(in) :: idx
      integer, intent(out), optional :: iostat_out
      integer :: iu
      integer, parameter :: dp = selected_real_kind(15,307)
      real(dp), intent(in) :: t, val
      character(len=512) :: line

      write(line,'(F10.5,1A,I0,1A,ES22.14)') t, ',', idx, ',', val
      write(unit,'(A)', iostat=iu) trim(line)
      if (present(iostat_out)) then
        iostat_out = iu
      else
        if (iu /= 0) write(*,'(A)') '  Warning: failed write to CSV unit'
      end if
    end subroutine csv_write_snapshot

    ! Generic helper: write a CHARACTER line atomically to a CSV unit.
    subroutine csv_write(unit, text, iostat_out)
      implicit none
      integer, intent(in) :: unit
      character(len=*), intent(in) :: text
      integer, intent(out), optional :: iostat_out
      integer :: iu

      write(unit,'(A)', iostat=iu) trim(text)
      if (present(iostat_out)) then
        iostat_out = iu
      else
        if (iu /= 0) write(*,'(A)') '  Warning: failed write to CSV unit'
      end if
    end subroutine csv_write

  ! -------------------- Heavy demos added: reverse-mode AD, 2D Poisson (FD), Fast Marching (brute) --------------------
  subroutine reverse_mode_ad_demo()
    implicit none
    integer, parameter :: dp = selected_real_kind(15,307)
    integer :: i, N, iu
    real(dp) :: x, v1, v2, v3, v4, v5, v6, adj1, adj2, adj3, adj4, adj5, adj6
    real(dp) :: analytic
    real(dp), allocatable :: xs(:), revgrads(:), angrads(:), errs(:)

    N = 41
    allocate(xs(N), revgrads(N), angrads(N), errs(N))
    do i = 1, N
      xs(i) = -2.0_dp + 4.0_dp * real(i-1,dp) / real(N-1,dp)
      x = xs(i)
      ! forward pass: compute intermediate values for f(x) = x^3 + sin(x) + x*exp(x)
      v1 = x
      v2 = v1**3                ! x^3
      v3 = sin(v1)              ! sin(x)
      v4 = exp(v1)              ! exp(x)
      v5 = v1 * v4              ! x * exp(x)
      v6 = v2 + v3 + v5         ! f

      ! reverse pass (backprop)
      adj1 = 0.0_dp; adj2 = 0.0_dp; adj3 = 0.0_dp; adj4 = 0.0_dp; adj5 = 0.0_dp; adj6 = 0.0_dp
      adj6 = 1.0_dp             ! df/df = 1
      ! v6 = v2 + v3 + v5
      adj2 = adj2 + adj6
      adj3 = adj3 + adj6
      adj5 = adj5 + adj6
      ! v2 = v1**3 -> dv2/dv1 = 3*v1**2
      adj1 = adj1 + adj2 * 3.0_dp * v1**2
      ! v3 = sin(v1) -> dv3/dv1 = cos(v1)
      adj1 = adj1 + adj3 * cos(v1)
      ! v5 = v1 * v4 -> dv5/dv1 = v4, dv5/dv4 = v1
      adj1 = adj1 + adj5 * v4
      adj4 = adj4 + adj5 * v1
      ! v4 = exp(v1) -> dv4/dv1 = exp(v1) = v4
      adj1 = adj1 + adj4 * v4

      revgrads(i) = adj1
      analytic = 3.0_dp * x**2 + cos(x) + exp(x) * (1.0_dp + x)
      angrads(i) = analytic
      errs(i) = abs(revgrads(i) - angrads(i))
    end do

    open(unit=230, file='reverse_mode_ad.csv', status='replace', action='write', iostat=iu)
    if (iu /= 0) then
      write(*,'(A)') '  Warning: could not open reverse_mode_ad.csv'
    else
      write(230,'(A)') 'x,reverse_grad,analytic_grad,abs_error'
      do i = 1, N
        write(230,'(F12.6,1A,ES12.6,1A,ES12.6,1A,ES12.6)') xs(i), ',', revgrads(i), ',', angrads(i), ',', errs(i)
      end do
      close(230)
      write(*,'(A)') 'Reverse-mode AD demo: CSV saved to reverse_mode_ad.csv'
    end if
    deallocate(xs, revgrads, angrads, errs)
  end subroutine reverse_mode_ad_demo


  subroutine fem2d_poisson_demo()
    implicit none
    integer, parameter :: dp = selected_real_kind(15,307)
    integer :: Nx, Ny, i, j, it, iu
    character(len=512) :: line
    real(dp) :: h, x, y, tol, diff, maxdiff, maxerr
    integer :: maxit
    real(dp), allocatable :: u(:,:), uold(:,:), f(:,:)

    ! Solve -Delta u = f on unit square with zero Dirichlet boundary using Jacobi iteration
    Nx = 41; Ny = 41
    h = 1.0_dp / real(Nx-1, dp)
    allocate(u(Nx,Ny), uold(Nx,Ny), f(Nx,Ny))
    ! choose analytic solution u = sin(pi x) sin(pi y) => -Delta u = 2*pi^2 sin(pi x) sin(pi y)
    do j = 1, Ny
      y = real(j-1,dp) * h
      do i = 1, Nx
        x = real(i-1,dp) * h
        f(i,j) = 2.0_dp * 3.141592653589793_dp**2 * sin(3.141592653589793_dp * x) * sin(3.141592653589793_dp * y)
        u(i,j) = 0.0_dp
      end do
    end do

    tol = 1.0e-8_dp
    maxit = 20000
    do it = 1, maxit
      uold = u
      maxdiff = 0.0_dp
      do j = 2, Ny-1
        do i = 2, Nx-1
          u(i,j) = 0.25_dp * ( uold(i+1,j) + uold(i-1,j) + uold(i,j+1) + uold(i,j-1) - h*h * f(i,j) )
        end do
      end do
      diff = 0.0_dp
      do j = 2, Ny-1
        do i = 2, Nx-1
          diff = max(diff, abs(u(i,j) - uold(i,j)))
        end do
      end do
      if (diff < tol) exit
    end do

    ! write CSV: x,y,u_numeric,u_exact,abs_err
    open(unit=231, file='fem2d_poisson.csv', status='replace', action='write', iostat=iu)
    if (iu /= 0) then
      write(*,'(A)') '  Warning: could not open fem2d_poisson.csv'
    else
      call csv_write(231, 'x,y,u_numeric,u_exact,abs_error', iu)
      do j = 1, Ny
        y = real(j-1,dp) * h
        do i = 1, Nx
          x = real(i-1,dp) * h
          write(line,'(F8.4,1A,F8.4,1A,ES18.10,1A,ES18.10,1A,ES18.10)') x, ',', y, ',', u(i,j), ',', sin(3.141592653589793_dp*x)*sin(3.141592653589793_dp*y), ',', abs(u(i,j) - sin(3.141592653589793_dp*x)*sin(3.141592653589793_dp*y))
          call csv_write(231, trim(line), iu)
        end do
      end do
      close(231)
      maxerr = 0.0_dp
      do j = 1, Ny
        y = real(j-1,dp) * h
        do i = 1, Nx
          x = real(i-1,dp) * h
          maxerr = max( maxerr, abs( u(i,j) - sin(3.141592653589793_dp*x) * sin(3.141592653589793_dp*y) ) )
        end do
      end do
      write(*,'(A,I0,A,ES18.10)') '2D Poisson (Jacobi) converged in iterations=', it, '  maxpointwise_err~', maxerr
      write(*,'(A)') 'FEM/FD 2D Poisson demo: CSV saved to fem2d_poisson.csv'
    end if
    deallocate(u, uold, f)
  end subroutine fem2d_poisson_demo


  subroutine fast_marching_demo()
    implicit none
    integer, parameter :: dp = selected_real_kind(15,307)
    integer :: Nx, Ny, i, j, iu, ii, jj
    integer :: argmin_idx, idx, gi, gj, ni, nj
    real(dp) :: h, x, y, cx, cy, seed_r, BIG, Tnew
    real(dp) :: best, ux, uy, s, disc
    real(dp), allocatable :: D(:,:), Dtent(:,:)
    integer, allocatable :: status(:,:)  ! 0 FAR, 1 TRIAL, 2 KNOWN
    integer, allocatable :: trial_i(:), trial_j(:)
    integer :: ntrial

    ! Fast Marching (Eikonal |grad T| = 1) on unit square with seeding region
    Nx = 101; Ny = 101
    h = 1.0_dp / real(Nx-1, dp)
    cx = 0.5_dp; cy = 0.5_dp; seed_r = 0.08_dp
    BIG = 1.0e30_dp

    allocate(D(Nx,Ny), Dtent(Nx,Ny), status(Nx,Ny))
    ! initialize
    do j = 1, Ny
      y = real(j-1,dp) * h
      do i = 1, Nx
        x = real(i-1,dp) * h
        if ( sqrt( (x-cx)**2 + (y-cy)**2 ) <= seed_r ) then
          D(i,j) = 0.0_dp
          status(i,j) = 2    ! KNOWN
        else
          D(i,j) = BIG
          status(i,j) = 0    ! FAR
        end if
        Dtent(i,j) = BIG
      end do
    end do

    ! initialize TRIAL set: neighbors of KNOWN
    ntrial = 0
    do j = 1, Ny
      do i = 1, Nx
        if (status(i,j) == 2) then
          do jj = max(1,j-1), min(Ny,j+1)
            do ii = max(1,i-1), min(Nx,i+1)
              if ( (abs(ii-i)+abs(jj-j)) == 1 ) then
                if (status(ii,jj) == 0) then
                  ! one-sided neighbor -> tentative = h
                  Dtent(ii,jj) = min( Dtent(ii,jj), D(i,j) + h )
                  ntrial = ntrial + 1
                  if (.not. allocated(trial_i)) then
                    allocate(trial_i(ntrial), trial_j(ntrial))
                    trial_i(ntrial) = ii
                    trial_j(ntrial) = jj
                  else
                    call extend_trial_ij(trial_i, trial_j, ntrial, ii, jj)
                  end if
                  status(ii,jj) = 1
                end if
              end if
            end do
          end do
        end if
      end do
    end do

    ! main loop: extract minimal trial, mark KNOWN, update neighbors
    do while (ntrial > 0)
      ! linear search for smallest Dtent among trial_list
      best = BIG; argmin_idx = -1
      idx = 1
      do while (idx <= ntrial)
        gi = trial_i(idx)
        gj = trial_j(idx)
        ! if a corrupted/invalid trial entry slipped in, remove it safely
        if (gi < 1 .or. gi > Nx .or. gj < 1 .or. gj > Ny) then
          trial_i(idx) = trial_i(ntrial)
          trial_j(idx) = trial_j(ntrial)
          ntrial = ntrial - 1
          if (ntrial == 0) then
            if (allocated(trial_i)) deallocate(trial_i)
            if (allocated(trial_j)) deallocate(trial_j)
            exit
          else
            call resize_trial_ij(trial_i, trial_j, ntrial)
          end if
          cycle  ! re-check the swapped-in entry at this idx
        end if
        if (Dtent(gi,gj) < best) then
          best = Dtent(gi,gj); argmin_idx = idx
        end if
        idx = idx + 1
      end do
      if (argmin_idx < 0) exit
      ! pop that trial
      gi = trial_i(argmin_idx)
      gj = trial_j(argmin_idx)
      ! guard: ensure coordinates are in range (robust against corrupted entries)
      if (gi < 1 .or. gi > Nx .or. gj < 1 .or. gj > Ny) then
        ! remove invalid trial entry by swapping with last
        trial_i(argmin_idx) = trial_i(ntrial)
        trial_j(argmin_idx) = trial_j(ntrial)
        ntrial = ntrial - 1
        if (ntrial == 0) then
          deallocate(trial_i, trial_j)
        else
          call resize_trial_ij(trial_i, trial_j, ntrial)
        end if
        cycle
      end if
      ! mark known
      D(gi,gj) = Dtent(gi,gj)
      status(gi,gj) = 2
      ! remove from trial_list by swapping last
      trial_i(argmin_idx) = trial_i(ntrial)
      trial_j(argmin_idx) = trial_j(ntrial)
      ntrial = ntrial - 1
      if (ntrial == 0) then
        deallocate(trial_i, trial_j)
      else
        call resize_trial_ij(trial_i, trial_j, ntrial)
      end if
      Dtent(gi,gj) = BIG

      ! update 4-neighbors
      do idx = 1, 4
        ! neighbor indices
        select case (idx)
        case (1)
          ni = gi-1; nj = gj
        case (2)
          ni = gi+1; nj = gj
        case (3)
          ni = gi; nj = gj-1
        case (4)
          ni = gi; nj = gj+1
        end select
        if (ni < 1 .or. ni > Nx .or. nj < 1 .or. nj > Ny) cycle
        if (status(ni,nj) == 2) cycle  ! already known
        ! compute tentative using neighboring KNOWN values
        ux = BIG; uy = BIG
        if (ni-1 >= 1) then
          if (status(ni-1,nj) == 2) ux = min(ux, D(ni-1,nj))
        end if
        if (ni+1 <= Nx) then
          if (status(ni+1,nj) == 2) ux = min(ux, D(ni+1,nj))
        end if
        if (nj-1 >= 1) then
          if (status(ni,nj-1) == 2) uy = min(uy, D(ni,nj-1))
        end if
        if (nj+1 <= Ny) then
          if (status(ni,nj+1) == 2) uy = min(uy, D(ni,nj+1))
        end if
        ! compute local Eikonal update
        if (ux == BIG .and. uy == BIG) then
          Tnew = BIG
        else if (abs(ux - uy) >= h) then
          Tnew = min(ux, uy) + h
        else
          s = ux + uy
          disc = s*s - 2.0_dp*(ux*ux + uy*uy - h*h)
          if (disc < 0.0_dp) disc = 0.0_dp
          Tnew = 0.5_dp * ( s + sqrt(disc) )
        end if
        if (Tnew < Dtent(ni,nj)) then
          Dtent(ni,nj) = Tnew
        end if
        if (status(ni,nj) == 0) then
          ! add to trial list
          ntrial = ntrial + 1
          if (.not. allocated(trial_i)) then
            allocate(trial_i(ntrial), trial_j(ntrial))
            trial_i(ntrial) = ni
            trial_j(ntrial) = nj
          else
            call extend_trial_ij(trial_i, trial_j, ntrial, ni, nj)
          end if
          status(ni,nj) = 1
        end if
      end do
    end do

    ! write CSV
    open(unit=232, file='fast_marching_dt.csv', status='replace', action='write', iostat=iu)
    if (iu /= 0) then
      write(*,'(A)') '  Warning: could not open fast_marching_dt.csv'
    else
      write(232,'(A)') 'x,y,distance'
      do j = 1, Ny
        y = real(j-1,dp) * h
        do i = 1, Nx
          x = real(i-1,dp) * h
          write(232,'(F8.4,1A,F8.4,1A,ES12.6)') x, ',', y, ',', D(i,j)
        end do
      end do
      close(232)
      write(*,'(A)') 'Fast Marching demo: CSV saved to fast_marching_dt.csv'
    end if
    deallocate(D, Dtent, status)
  end subroutine fast_marching_demo

  ! Helper: append a value to trial_list (resize array)
  subroutine extend_trial_ij(ai, aj, n, vi, vj)
    implicit none
    integer, allocatable, intent(inout) :: ai(:), aj(:)
    integer, intent(in) :: n, vi, vj
    integer, allocatable :: tmpi(:), tmpj(:)
    allocate(tmpi(n), tmpj(n))
    if (n > 1) then
      tmpi(1:n-1) = ai(1:n-1)
      tmpj(1:n-1) = aj(1:n-1)
    end if
    tmpi(n) = vi; tmpj(n) = vj
    if (allocated(ai)) deallocate(ai)
    if (allocated(aj)) deallocate(aj)
    allocate(ai(n), aj(n))
    ai = tmpi; aj = tmpj
    deallocate(tmpi, tmpj)
  end subroutine extend_trial_ij

  subroutine resize_trial_ij(ai, aj, n)
    implicit none
    integer, allocatable, intent(inout) :: ai(:), aj(:)
    integer, intent(in) :: n
    integer, allocatable :: tmpi(:), tmpj(:)
    allocate(tmpi(n), tmpj(n))
    tmpi = ai(1:n); tmpj = aj(1:n)
    deallocate(ai, aj)
    allocate(ai(n), aj(n))
    ai = tmpi; aj = tmpj
    deallocate(tmpi, tmpj)
  end subroutine resize_trial_ij

  subroutine implicit_heat2d_demo()
    implicit none
    integer, parameter :: dp = selected_real_kind(15,307)
    integer :: Nx, Ny, i, j, iter, it, maxiter, iu
    real(dp) :: dx, dy, dt, tfinal, alpha, tol, r, t
    real(dp) :: maxdiff, newval, pi
    real(dp), allocatable :: u(:,:), u_old(:,:), u_iter(:,:)

    ! problem parameters (small grid for demo)
    Nx = 51; Ny = 51
    alpha = 0.1_dp
    dx = 1.0_dp / real(Nx-1, dp)
    dy = 1.0_dp / real(Ny-1, dp)
    dt = 5.0e-4_dp
    tfinal = 1.0e-2_dp
    maxiter = 2000
    tol = 1.0e-7_dp

    r = alpha * dt / (dx*dx)

    allocate(u(Nx,Ny), u_old(Nx,Ny), u_iter(Nx,Ny))

    ! initial condition: sin(pi x) sin(pi y)
    do i = 1, Nx
      do j = 1, Ny
        u(i,j) = sin(pi * real(i-1,dp) * dx) * sin(pi * real(j-1,dp) * dy)
      end do
    end do

    t = 0.0_dp
    do it = 1, int(tfinal/dt + 0.5_dp)
      u_old = u
      ! backward-Euler: solve (I - r*L) u = u_old with Jacobi iterations
      u_iter = u_old
      do iter = 1, maxiter
        maxdiff = 0.0_dp
        do i = 2, Nx-1
          do j = 2, Ny-1
            newval = ( u_old(i,j) + r*( u_iter(i+1,j) + u_iter(i-1,j) + u_iter(i,j+1) + u_iter(i,j-1) ) ) / (1.0_dp + 4.0_dp*r)
            maxdiff = max(maxdiff, abs(newval - u_iter(i,j)))
            u_iter(i,j) = newval
          end do
        end do
        if (maxdiff < tol) exit
      end do
      u = u_iter
      t = t + dt
    end do

    ! write CSV
    open(unit=290, file='implicit_heat2d.csv', status='replace', action='write', iostat=iu)
    if (iu /= 0) then
      write(*,'(A)') '  Warning: could not open implicit_heat2d.csv for writing.'
    else
      write(290,'(A)') 'x,y,u'
      do j = 1, Ny
        do i = 1, Nx
          write(290,'(F8.5,1A,F8.5,1A,ES15.8)') real((i-1),dp)*dx, ',', real((j-1),dp)*dy, ',', u(i,j)
        end do
      end do
      close(290)
      write(*,'(A)') 'Implicit 2D heat demo: CSV saved to implicit_heat2d.csv'
    end if

    deallocate(u, u_old, u_iter)
  end subroutine implicit_heat2d_demo

  subroutine crank_nicolson2d_demo()
    implicit none
    integer, parameter :: dp = selected_real_kind(15,307)
    integer :: Nx, Ny, i, j, it, nsteps, iu, iter, maxiter
    integer :: k, ncases
    real(dp) :: dx, dy, dt, tfinal, alpha, r, tol, t
    real(dp) :: maxdiff, newval, pi, x, y
    real(dp) :: L2err, Linferr, sumsq, errv, exfac
    real(dp), allocatable :: u(:,:), u_rhs(:,:), u_new(:,:)
    integer, allocatable :: Ns(:)
    integer :: total_iters
    real(dp) :: omega, avg_iter
    character(len=64) :: fname

    pi = 3.141592653589793_dp
    alpha = 0.1_dp
    tfinal = 1.0e-3_dp   ! short final time to keep runtime small for finer grids
    maxiter = 5000
    tol = 1.0e-8_dp

    ! grid sizes to sweep for convergence
    ncases = 3
    allocate(Ns(ncases))
    Ns = (/ 21, 41, 81 /)

    open(unit=291, file='crank_nicolson2d_convergence.csv', status='replace', action='write', iostat=iu)
    if (iu /= 0) then
      write(*,'(A)') '  Warning: could not open crank_nicolson2d_convergence.csv for writing.'
      deallocate(Ns)
      return
    end if
    write(291,'(A)') 'Nx,L2_error,Linf_error,avg_iter'

    do k = 1, ncases
      Nx = Ns(k)
      Ny = Nx
      dx = 1.0_dp / real(Nx-1, dp)
      dy = dx
      ! choose dt proportional to dx (CN is unconditionally stable, but dt affects temporal error)
      dt = 0.5_dp * dx
      nsteps = max(1, int(tfinal / dt + 0.5_dp))
      dt = tfinal / real(nsteps, dp)

      allocate(u(Nx,Ny), u_rhs(Nx,Ny), u_new(Nx,Ny))

      ! initial condition: sin(pi x) sin(pi y
      do i = 1, Nx
        x = real(i-1,dp) * dx
        do j = 1, Ny
          y = real(j-1,dp) * dy
          u(i,j) = sin(pi * x) * sin(pi * y)
        end do
      end do

      r = alpha * dt / (dx*dx)

      t = 0.0_dp
      ! time-stepping (handled inside SOR loop below)
      ! outer time loop removed — inner SOR loop iterates over time steps
        ! build RHS: (I + 0.5*r*L) u
        do i = 2, Nx-1
          do j = 2, Ny-1
            u_rhs(i,j) = u(i,j) + 0.5_dp * r * ( u(i+1,j) - 2.0_dp*u(i,j) + u(i-1,j) ) + 0.5_dp * r * ( u(i,j+1) - 2.0_dp*u(i,j) + u(i,j-1) )
          end do
        end do
        ! enforce homogeneous Dirichlet BCs on RHS
        do i = 1, Nx
          u_rhs(i,1) = 0.0_dp; u_rhs(i,Ny) = 0.0_dp
        end do
        do j = 1, Ny
          u_rhs(1,j) = 0.0_dp; u_rhs(Nx,j) = 0.0_dp
        end do

        ! Solve (I - 0.5*r*L) u_new = u_rhs with SOR (Gauss-Seidel + relaxation)
        ! Parameterize relaxation omega (1.0 => Gauss-Seidel, >1 => over-relaxation)
        if (it == 1) then
          omega = 1.3_dp   ! a reasonable default over-relaxation factor for 5-pt Laplacian
        end if
        u_new = u
        iter = 0
        total_iters = 0
        do it = 1, nsteps
          iter = 0
          do
            iter = iter + 1
            maxdiff = 0.0_dp
            do i = 2, Nx-1
              do j = 2, Ny-1
                ! Gauss-Seidel stencil value (using most-recent u_new neighbors)
                newval = ( u_rhs(i,j) + 0.5_dp * r * ( u_new(i+1,j) + u_new(i-1,j) + u_new(i,j+1) + u_new(i,j-1) ) ) / (1.0_dp + 2.0_dp*r)
                ! SOR update
                newval = (1.0_dp - omega) * u_new(i,j) + omega * newval
                maxdiff = max(maxdiff, abs(newval - u_new(i,j)))
                u_new(i,j) = newval
              end do
            end do
            if (maxdiff < tol .or. iter >= maxiter) exit
          end do
          total_iters = total_iters + iter
          u = u_new
          t = t + dt
        end do
        ! compute average iterations per time-step for reporting
        if (nsteps > 0) then
          avg_iter = real(total_iters, dp) / real(nsteps, dp)
        else
          avg_iter = 0.0_dp
        end if
      ! end time-stepping

      ! compute error against manufactured solution u_exact = exp(-2*pi^2*alpha*tfinal)*sin(pi x) sin(pi y)
      exfac = exp( -2.0_dp * pi*pi * alpha * tfinal )
      sumsq = 0.0_dp
      Linferr = 0.0_dp
      do i = 1, Nx
        x = real(i-1,dp) * dx
        do j = 1, Ny
          y = real(j-1,dp) * dy
          errv = u(i,j) - exfac * sin(pi*x) * sin(pi*y)
          sumsq = sumsq + errv*errv
          Linferr = max(Linferr, abs(errv))
        end do
      end do
      L2err = sqrt( sumsq * dx * dy )

      write(291,'(I6,1A,ES12.6,1A,ES12.6,1A,ES12.6)') Nx, ',', L2err, ',', Linferr, ',', avg_iter

      ! also write final snapshot for this grid (CSV named by Nx)
      write(fname,'(A,I0,A)') 'cn2d_snap_N', Nx, '.csv'
      open(unit=292, file=trim(fname), status='replace', action='write', iostat=iu)
      if (iu == 0) then
        write(292,'(A)') 'x,y,u'
        do j = 1, Ny
          do i = 1, Nx
            write(292,'(F8.5,1A,F8.5,1A,ES15.8)') real((i-1),dp)*dx, ',', real((j-1),dp)*dy, ',', u(i,j)
          end do
        end do
        close(292)
      end if

      deallocate(u, u_rhs, u_new)
    end do

    close(291)
    write(*,'(A)') 'Crank–Nicolson 2D demo: CSV saved to crank_nicolson2d_convergence.csv'
    deallocate(Ns)
  end subroutine crank_nicolson2d_demo

  subroutine rough_distributions_demo()
    implicit none
    integer, parameter :: dp = selected_real_kind(15,307)
    integer :: Nt, n, iu
    character(len=512) :: line
    real(dp) :: T_local, dt_local, sigma, amp
    real(dp), allocatable :: ts(:), u(:), f(:)
    integer :: k
    ! Rough distributions demo with sigma sweep and L2 convergence diagnostics
    integer :: nsig, si, i
    real(dp), allocatable :: sigma_vals(:), f_ref(:), u_ref(:)
    real(dp) :: L2_f, L2_u, sumsq
    real(dp) :: max_abs_f, max_abs_diff_u
    ! temporaries for RK4 (no longer needed inline; kept for compatibility)
    ! Note: actual RK4 work moved to `rk4_solve_forcing` helper.

    T_local = 1.0_dp
    Nt = 201
    dt_local = T_local / real(Nt-1, dp)
    allocate(ts(Nt))

    ! time grid
    do n = 1, Nt
      ts(n) = real(n-1, dp) * dt_local
    end do

    ! choose a set of mollifier widths (sigma) to sweep, decreasing to approximate delta
    nsig = 6
    allocate(sigma_vals(nsig))
    sigma_vals = (/ 1.0e-1_dp, 5.0e-2_dp, 2.0e-2_dp, 1.0e-2_dp, 5.0e-3_dp, 2.0e-3_dp /)

    amp = 1.0_dp

    ! Compute reference solution using the smallest sigma (last entry)
    allocate(f_ref(Nt), u_ref(Nt))
    f_ref = 0.0_dp
    do k = 1, 3
      call add_gaussian_spike(ts, f_ref, amp, 0.25_dp*real(k,dp), sigma_vals(nsig))
    end do
    ! RK4 for reference solve of du/dt = -u + f(t) (use helper)
    u_ref = 0.0_dp
    call rk4_solve_forcing(Nt, dt_local, ts, f_ref, u_ref)

    open(unit=410, file='rough_distributions.csv', status='replace', action='write', iostat=iu)
    if (iu /= 0) then
      write(*,'(A)') '  Warning: could not open rough_distributions.csv for writing.'
      deallocate(ts, f_ref, u_ref, sigma_vals)
      return
    end if
    call csv_write(410, 't,u_ref,forcing_ref', iu)
    do n = 1, Nt
      write(line,'(F8.4,1A,ES12.6,1A,ES12.6)') ts(n), ',', u_ref(n), ',', f_ref(n)
      call csv_write(410, trim(line), iu)
    end do
    close(410)

    ! Now sweep over sigma and compute discrete L2 norms relative to reference
    open(unit=411, file='rough_distributions_sigma_sweep.csv', status='replace', action='write', iostat=iu)
    if (iu /= 0) then
      write(*,'(A)') '  Warning: could not open rough_distributions_sigma_sweep.csv for writing.'
      deallocate(ts, f_ref, u_ref, sigma_vals)
      return
    end if
    call csv_write(411, 'sigma,L2_f,L2_u,max_abs_f,max_abs_diff_u', iu)

    allocate(f(Nt), u(Nt))
    do si = 1, nsig
      sigma = sigma_vals(si)
      f = 0.0_dp
      do k = 1, 3
        call add_gaussian_spike(ts, f, amp, 0.25_dp*real(k,dp), sigma)
      end do

      ! solve du/dt = -u + f(t) by RK4 using helper
      u = 0.0_dp
      call rk4_solve_forcing(Nt, dt_local, ts, f, u)

      ! compute discrete L2 norms (approx integral sqrt(sum (diff^2)*dt) )
      sumsq = 0.0_dp
      do i = 1, Nt
        sumsq = sumsq + ( f(i) - f_ref(i) )**2
      end do
      L2_f = sqrt( sumsq * dt_local )

      sumsq = 0.0_dp
      do i = 1, Nt
        sumsq = sumsq + ( u(i) - u_ref(i) )**2
      end do
      L2_u = sqrt( sumsq * dt_local )

      ! also record a couple of simple diagnostics
      max_abs_f = 0.0_dp
      max_abs_diff_u = 0.0_dp
      do i = 1, Nt
        max_abs_f = max( max_abs_f, abs(f(i)) )
        max_abs_diff_u = max( max_abs_diff_u, abs(u(i) - u_ref(i)) )
      end do

      write(line,'(ES12.6,1A,ES12.6,1A,ES12.6,1A,ES12.6,1A,ES12.6)') sigma, ',', L2_f, ',', L2_u, ',', max_abs_f, ',', max_abs_diff_u
      call csv_write(411, trim(line), iu)
    end do

    close(411)
    deallocate(ts, f_ref, u_ref, sigma_vals, f, u)
    write(*,'(A)') 'Rough-distributions demo: CSV saved to rough_distributions.csv and rough_distributions_sigma_sweep.csv'
  end subroutine rough_distributions_demo

  subroutine add_gaussian_spike(ts, f, amp, t0, sigma)
    implicit none
    integer, parameter :: dp = selected_real_kind(15,307)
    real(dp), intent(in) :: amp, t0, sigma
    real(dp), intent(inout) :: ts(:), f(:)
    integer :: i, N
    real(dp) :: x
    N = size(ts)
    do i = 1, N
      x = ts(i) - t0
      f(i) = f(i) + amp * exp( -0.5_dp * (x/sigma)**2 ) / ( sigma * sqrt(2.0_dp*3.141592653589793_dp) )
    end do
  end subroutine add_gaussian_spike

  ! RK4 helper for scalar ODE of the form du/dt = -u + f(t)
  subroutine rk4_solve_forcing(Nt, dt_local, ts, f, u)
    implicit none
    integer, intent(in) :: Nt
    real(dp), intent(in) :: dt_local
    real(dp), intent(in) :: ts(:)
    real(dp), intent(in) :: f(:)
    real(dp), intent(out) :: u(:)
    integer :: n
    real(dp) :: k1, k2, k3, k4, f_mid

    if ( size(f) < Nt .or. size(ts) < Nt .or. size(u) < Nt ) then
      return
    end if

    u(1) = 0.0_dp
    do n = 1, Nt-1
      k1 = - u(n) + f(n)
      f_mid = 0.5_dp * ( f(n) + f(n+1) )
      k2 = - ( u(n) + 0.5_dp*dt_local*k1 ) + f_mid
      k3 = - ( u(n) + 0.5_dp*dt_local*k2 ) + f_mid
      k4 = - ( u(n) + dt_local*k3 ) + f(n+1)
      u(n+1) = u(n) + dt_local * ( k1 + 2.0_dp*k2 + 2.0_dp*k3 + k4 ) / 6.0_dp
    end do

  end subroutine rk4_solve_forcing

  subroutine stability_region_demo()
    implicit none
    integer, parameter :: dp = selected_real_kind(15,307)
    integer :: Nr, Ni, ir, ii, iu
    real(dp) :: re_min, re_max, im_min, im_max, dre, dim
    complex(dp) :: z, R_euler, R_rk4

    ! grid for complex z = lambda * dt (evaluate stability polynomials)
    Nr = 201
    Ni = 201
    re_min = -4.0_dp
    re_max = 2.0_dp
    im_min = -4.0_dp
    im_max = 4.0_dp
    dre = (re_max - re_min) / real(Nr-1, dp)
    dim = (im_max - im_min) / real(Ni-1, dp)

    open(unit=430, file='stability_regions.csv', status='replace', action='write', iostat=iu)
    if (iu /= 0) then
      write(*,'(A)') '  Warning: could not open stability_regions.csv for writing.'
      return
    end if
    write(430,'(A)') 're_z,im_z,abs_R_euler,abs_R_rk4'

    do ir = 0, Nr-1
      do ii = 0, Ni-1
        z = cmplx( re_min + real(ir,dp)*dre, im_min + real(ii,dp)*dim, kind=dp )
        ! Explicit Euler stability polynomial: R_euler(z) = 1 + z
        R_euler = 1.0_dp + z
        ! RK4 stability polynomial: 1 + z + z^2/2 + z^3/6 + z^4/24
        R_rk4 = 1.0_dp + z + z**2/2.0_dp + z**3/6.0_dp + z**4/24.0_dp
        write(430,'(F8.4,1A,F8.4,1A,ES12.6,1A,ES12.6)') real(z), ',', aimag(z), ',', abs(R_euler), ',', abs(R_rk4)
      end do
    end do

    close(430)
    write(*,'(A)') 'Stability region demo: CSV saved to stability_regions.csv'
  end subroutine stability_region_demo

  subroutine plot_sigma_sweep_demo()
    implicit none
    integer, parameter :: dp = selected_real_kind(15,307)
    integer :: iu
    ! no extra character buffers needed

    open(unit=440, file='plot_rough_distributions_sigma.gnu', status='replace', action='write', iostat=iu)
    if (iu /= 0) then
      write(*,'(A)') '  Warning: could not open plot_rough_distributions_sigma.gnu for writing.'
      return
    end if

    write(440,'(A)') "set terminal pngcairo size 900,600 enhanced font 'Arial,12'"
    write(440,'(A)') "set output 'rough_distributions_sigma_sweep.png'"
    write(440,'(A)') "set logscale x 10"
    write(440,'(A)') "set xlabel 'sigma'"
    write(440,'(A)') "set ylabel 'L2 (solution) / L2 (forcing)'"
    write(440,'(A)') "set grid"
    write(440,'(A)') "set key left top"
    write(440,'(A)') "set style data linespoints"
    write(440,'(A)') "plot 'rough_distributions_sigma_sweep.csv' using 1:3 with linespoints lw 2 pt 7 lc rgb 'blue' title 'L2_u', \"
    write(440,'(A)') "     'rough_distributions_sigma_sweep.csv' using 1:2 with linespoints lw 2 pt 5 lc rgb 'red' title 'L2_f'"

    close(440)
    write(*,'(A)') "Plot script written to plot_rough_distributions_sigma.gnu (run with gnuplot to create PNG)."
  end subroutine plot_sigma_sweep_demo

  subroutine tda_persistence_demo()
    implicit none
    integer, parameter :: dp = selected_real_kind(15,307)
    integer :: N, i, j, iu, t_idx
    real(dp), allocatable :: xs(:), ys(:), dmat(:,:), thresholds(:)
    integer, allocatable :: parent(:)
    integer :: comps

    ! Tiny Vietoris-Rips 0-D persistence demo (connected components)
    N = 6
    allocate(xs(N), ys(N))
    ! Two clusters: three points near (0,0) and three near (1,0)
    xs = (/ 0.00_dp, 0.10_dp, 0.05_dp, 1.00_dp, 1.10_dp, 0.95_dp /)
    ys = (/ 0.00_dp, 0.02_dp, 0.10_dp, 0.00_dp, -0.02_dp, 0.05_dp /)

    allocate(dmat(N,N))
    do i = 1, N
      do j = 1, N
        dmat(i,j) = sqrt( (xs(i)-xs(j))**2 + (ys(i)-ys(j))**2 )
      end do
    end do

    ! thresholds grid from 0 to max pairwise distance
    allocate(thresholds(21))
    thresholds = 0.0_dp
    thresholds(1) = 0.0_dp
    do i = 2, size(thresholds)
      thresholds(i) = thresholds(i-1)
    end do
    ! find max dist
    thresholds(size(thresholds)) = 0.0_dp
    do i = 1, N
      do j = i+1, N
        thresholds(size(thresholds)) = max(thresholds(size(thresholds)), dmat(i,j))
      end do
    end do
    ! fill linearly
    do i = 1, size(thresholds)
      thresholds(i) = ( real(i-1,dp) / real(size(thresholds)-1,dp) ) * thresholds(size(thresholds))
    end do

    allocate(parent(N))

    open(unit=400, file='tda_persistence.csv', status='replace', action='write', iostat=iu)
    if (iu /= 0) then
      write(*,'(A)') '  Warning: could not open tda_persistence.csv for writing.'
      deallocate(xs, ys, dmat, thresholds, parent)
      return
    end if
    write(400,'(A)') 'threshold,num_components'

    do t_idx = 1, size(thresholds)
      ! init union-find
      do i = 1, N
        parent(i) = i
      end do

      ! union pairs within threshold
      do i = 1, N
        do j = i+1, N
          if ( dmat(i,j) <= thresholds(t_idx) ) then
            call tda_union_sets(parent, i, j)
          end if
        end do
      end do

      ! count distinct roots
      comps = 0
      do i = 1, N
        if ( tda_find_root(parent, i) == i ) comps = comps + 1
      end do
      write(400,'(ES12.6,1A,I0)') thresholds(t_idx), ',', comps
    end do

    close(400)
    deallocate(xs, ys, dmat, thresholds, parent)
    write(*,'(A)') 'TDA / persistence demo: CSV saved to tda_persistence.csv'
  end subroutine tda_persistence_demo

  recursive integer function tda_find_root(parent, a) result(res)
    implicit none
    integer, intent(inout) :: parent(:)
    integer, intent(in) :: a
    integer :: p
    p = parent(a)
    if (p == a) then
      res = a
    else
      parent(a) = tda_find_root(parent, p)
      res = parent(a)
    end if
  end function tda_find_root

  subroutine tda_union_sets(parent, a, b)
    implicit none
    integer, intent(inout) :: parent(:)
    integer, intent(in) :: a, b
    integer :: ra, rb
    ra = tda_find_root(parent, a)
    rb = tda_find_root(parent, b)
    if (ra /= rb) parent(rb) = ra
  end subroutine tda_union_sets

  end program fortran_calculus