Clustering Assignment 

Adrian Othon, adrian_othon@med.unc.edu  
Bios611

This project is looking at two clustering algorithms using a populated dataset to understand how cluster separability affects a particular algorithms performanse. 

Task1: K-means with Gap Statistic on hypercube clusters 

Task2: Spectral Clustering on concentric 3D shells 

My work runs reproducibly inside a Docker container with a Makefile for automation as requested in the assignment instructions. 

Reproducibility Instructions 

1: Build the docker image
	docker build -t ufo-assignment 

Username: rstudio
Password: rstudio 

2. To run the analysis 
	once inside R studio type "make" (without the quotes)

This will run the complete R script that has both tasks in it. 

3. Clean 
	type "make clean" (without the quotes) 

## Output Task 1 

figures/gap_est_k.png is the estimated clusters vs side length for different dimensions for task 1. 

The K-means performs well until cluster centers become too close relative to noise resulting in high dimensions failing earlier. 

## Output Task 2 

figures/spectral_est_k.png is the estimated clusters vs max radius for spectral clustering. 

The spectral clustering distinguishes shells effectively until the radii begin overlapping. Meaning, smalled d_threshold causes earlier failure.  
