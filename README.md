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
 
After building the Docker image:
docker run -it --rm \
  -v "$(pwd)":/home/rstudio/project \
  -w /home/rstudio/project \
  ufo-assignment bash

2. To run the analysis 
	Once in the container mounted to the correct folder type "make clean" followed by "make" on the terminal  (without the quotes)

This will run the complete R script that has both tasks in it. 


## Output Task 1 

The estimated number of clusters begins to drop when the side length falls below 3 in higher-dimensional data and 1 with lower-dimensional data. This occurs because as cluster centers move closeer relative to the noise, their distributions overlap. K-mean depends on Euclidean distance and assumes spherical, well-separated clusters, so once overlap occurs, it merges them. The Gap statistic correctly tracks this behavior by showing smaller gaps between real data and a uniform reference, indicating that the structure is no longer distinct. 

Cluster seperatibility determines when K-means fails with higher dimensions magnifying the overlap effects. 

figures/gap_est_k.png is the estimated clusters vs side length for different dimensions for task 1. 

## Output Task 2 

With d_threshold = 1, spectral clustering begins under detecting shells once the maximum radius falls below 6, where shells start overlapping. The normalized Laplacian's eigenvectors can no longer represent four disconnected components, so K-means groups adjacent shells together. If d_threshold were smaller than 1, edges in the similarity graph would become sparse and clusters would fragment earlier. That can also be stated as "failure at larger radii". If d_threshold were larger than 1, the graph would connect shells too easily resulting in early merging. 

Ultimately, spectral clusing relies on graph connectivity and its success depend on the spacing between shells and the chosen distance threshold. 
figures/spectral_est_k.png is the estimated clusters vs max radius for spectral clustering. 
  
