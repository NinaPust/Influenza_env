require(ape)
require(tidyverse)
require(MASS)
#Also requires MACSE
#macse_path = "macse_v2.07.jar"

download_align_trim <- function(data, align_input_file, align_output_file1, align_output_file2, trimmed_output_file, path_to_macse) {
  accession_numbers <- data$accession_number
  DNA_bin <- read.GenBank(accession_numbers) #downloads the sequences
  DNA_dups <- duplicated(as.list(DNA_bin))
  DNA_nondups <- as.list(DNA_bin)[!DNA_dups] 
  
  write.FASTA(DNA_nondups, file = paste0(align_input_file)) #Saves the FASTA file
  
  command<- paste0("java -jar ", path_to_macse," -prog alignSequences -max_refine_iter 0 -seq ", align_input_file, " -out_NT ", align_output_file1)
  system(command) #aligning 1
  #print(command)
  
  command2 <- paste0("java -jar ", path_to_macse, " -prog exportAlignment -align ", align_output_file1, " -codonForInternalStop NNN -codonForFinalStop --- -codonForInternalFS --- -charForRemainingFS - -out_NT ", align_output_file2)
  system(command2) #aligning 2
  #print(command2)
  aligned_DNA <- read.FASTA(paste0(align_output_file2)) #reads the aligned file into R
  aligned_DNA <- as.matrix(aligned_DNA)
  
  #trimming
  fasttree <- njs(dist.dna(aligned_DNA)) 
  fasttree$edge.length <- abs(fasttree$edge.length) #absolute because min value was negative
  
  trimmedtree <- trimLongTipBrs(fasttree, pval = 0.001)
  
  aligned_DNA <- aligned_DNA[trimmedtree$tip.label,]
  
  aligned_DNA_trimmed <- trimCols(aligned_DNA, prop = 0.5, codon = T)
  
  write.FASTA(aligned_DNA, file = paste0(trimmed_output_file))
 
  return(aligned_DNA_trimmed
         )
  
}

