set terminal pngcairo size 900,600 enhanced font 'Arial,12'
set output 'rough_distributions_sigma_sweep.png'
set logscale x 10
set xlabel 'sigma'
set ylabel 'L2 (solution) / L2 (forcing)'
set grid
set key left top
set style data linespoints
plot 'rough_distributions_sigma_sweep.csv' using 1:3 with linespoints lw 2 pt 7 lc rgb 'blue' title 'L2_u', \
     'rough_distributions_sigma_sweep.csv' using 1:2 with linespoints lw 2 pt 5 lc rgb 'red' title 'L2_f'
