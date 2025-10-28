.PHONY: all clean

all: figures/gap_est_k.png figures/spectral_est_k.png

figures/gap_est_k.png: scripts/task1_gap.R R/utils.R
	Rscript scripts/task1_gap.R

figures/spectral_est_k.png: scripts/task2_spectral.R R/utils.R
	Rscript scripts/task2_spectral.R

clean:
	rm -f figures/*.png
