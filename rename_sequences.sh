for i in `cat $1`;  do  on=$(echo $i | cut -d "," -f1);  nn=$(echo $i | cut -d "," -f2);  sed -i '' "s#$on#$nn#g" $2;  done


###example: 
###./rename_sequences.sh rename_dengue_all.txt Phylogenetics/Dengue/DENV2_global_genomes_above50_sequences_aln_nextalign_renamed_final.fasta
