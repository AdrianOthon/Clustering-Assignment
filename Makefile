.PHONY: all clean

all:
	mkdir -p figures
	Rscript Task1and2.R

clean:
	rm -f figures/*.png
