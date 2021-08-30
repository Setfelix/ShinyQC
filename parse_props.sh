#!/bin/sh

#$1 is properties directory
prop_dir=$1
#prop_dir=/media/e/test_dir/properties

[ "$prop_dir" == "" ] && { echo "Usage: parse_props.sh /path/to/properties/files"; exit 1; }

#parse_props $prop_dir


#function to convert properties file into multiple tsv files
parse_props () {
    #mkdir -p $1/temp
    #mkdir -p $1/out_dir
    for prop in $1/*.dedupedV2Duplex.properties;
    do
    filename=$(basename $prop)
    #| cut -f3 -d'-'
    sample_name=$(echo $filename | cut -f1 -d'.')
    #single strand consensus metrics
    #ss_read_pairs_per_bc
    #extract read_pairs_per_bc columns
    grep -m 1 READ_PAIRS_PER_BC_HISTOGRAM_X $prop | sed 's/=/,/' | sed 's/,/ /g' | awk ' { for(i=1;i<=NF;i++) { print $i } } ' > $1/temp/${sample_name}_ss_read_pairs_per_bc_hist_x.txt
    grep -m 1 READ_PAIRS_PER_BC_HISTOGRAM_Y $prop | sed 's/=/,/' | sed 's/,/ /g' | awk ' { for(i=1;i<=NF;i++) { print $i } } ' > $1/temp/${sample_name}_ss_read_pairs_per_bc_hist_y.txt
    #get number of lines from hist file for creating sample name column
    ss_read_pairs_per_bc_nlines=$(wc $1/temp/${sample_name}_ss_read_pairs_per_bc_hist_y.txt | awk '{ print $1 }')
    #create sample name column
    echo "sample" > $1/temp/${sample_name}_ss.txt
    for ((i=1; i<=$ss_read_pairs_per_bc_nlines-1; i++)); do echo $sample_name >> $1/temp/${sample_name}_ss.txt; done
    paste $1/temp/${sample_name}_ss.txt $1/temp/${sample_name}_ss_read_pairs_per_bc_hist_x.txt $1/temp/${sample_name}_ss_read_pairs_per_bc_hist_y.txt > $1/temp/${sample_name}_ss_read_pairs_per_bc_hist.tsv
    
    #duplex strand consensus metrics
    #ds_read_pairs_per_bc
    tail -n6 $prop | grep -m 1 READ_PAIRS_PER_BC_HISTOGRAM_X | sed 's/=/,/' | sed 's/,/ /g' | awk ' { for(i=1;i<=NF;i++) { print $i } } ' > $1/temp/${sample_name}_ds_read_pairs_per_bc_hist_x.txt
    tail -n6 $prop | grep -m 1 READ_PAIRS_PER_BC_HISTOGRAM_Y | sed 's/=/,/' | sed 's/,/ /g' | awk ' { for(i=1;i<=NF;i++) { print $i } } ' > $1/temp/${sample_name}_ds_read_pairs_per_bc_hist_y.txt
    ds_read_pairs_per_bc_nlines=$(wc $1/temp/${sample_name}_ds_read_pairs_per_bc_hist_x.txt | awk '{ print $1 }')
    echo "sample" > $1/temp/${sample_name}_ds.txt
    for ((i=1; i<=$ds_read_pairs_per_bc_nlines-1; i++)); do echo $sample_name >> $1/temp/${sample_name}_ds.txt; done
    paste $1/temp/${sample_name}_ds.txt $1/temp/${sample_name}_ds_read_pairs_per_bc_hist_x.txt $1/temp/${sample_name}_ds_read_pairs_per_bc_hist_y.txt > $1/temp/${sample_name}_ds_read_pairs_per_bc_hist.tsv
    
    #duplication rate
    grep DUPLICATE_RATE $prop | sed 's/=/,/' | sed 's/,/ /g' | awk ' { for(i=1;i<=NF;i++) { print $i } } ' > $1/temp/${sample_name}_ss_dup_rate.txt
    echo "sample" > $1/temp/${sample_name}_ss_dup_rate_name.txt
    echo $sample_name >> $1/temp/${sample_name}_ss_dup_rate_name.txt
    paste $1/temp/${sample_name}_ss_dup_rate_name.txt $1/temp/${sample_name}_ss_dup_rate.txt > $1/temp/${sample_name}_ss_duplication_rate.tsv
        
    #number of singleplex and duplex reads
    grep DUPLEX_VS_SINGLE_CONSENSUS_READS $prop | sed 's/=/,/' | sed 's/,/ /' | awk ' { for(i=1;i<=NF;i++) { print $i } } ' > $1/temp/${sample_name}_duplex_single_reads.txt
    echo "sample" > $1/temp/${sample_name}_nreads_name.txt
    echo $sample_name >> $1/temp/${sample_name}_nreads_name.txt
    paste $1/temp/${sample_name}_nreads_name.txt $1/temp/${sample_name}_duplex_single_reads.txt > $1/temp/${sample_name}_duplex_single_reads.tsv
    
    #check whether files were created for the sample
    if [[ -f "$1/temp/${sample_name}_ss_read_pairs_per_bc_hist.tsv" && -f "$1/temp/${sample_name}_ds_read_pairs_per_bc_hist.tsv" && -f "$1/temp/${sample_name}_ss_duplication_rate.tsv" && -f "$1/temp/${sample_name}_duplex_single_reads.tsv" ]]; then 
    echo ${sample_name}...done
    fi
    done
    
    ##aggregate duplex and single strand metrics across samples into single file
    cat $1/temp/*_ss_read_pairs_per_bc_hist.tsv | grep "^sample" | head -n 1 > $1/out_dir/all_samples_ss_read_pairs_per_bc_hist.tsv
    cat $1/temp/*_ss_read_pairs_per_bc_hist.tsv | grep -v "^sample" >> $1/out_dir/all_samples_ss_read_pairs_per_bc_hist.tsv
    
    cat $1/temp/*_ds_read_pairs_per_bc_hist.tsv | grep "^sample" | head -n 1 > $1/out_dir/all_samples_ds_read_pairs_per_bc_hist.tsv
    cat $1/temp/*_ds_read_pairs_per_bc_hist.tsv | grep -v "^sample" >> $1/out_dir/all_samples_ds_read_pairs_per_bc_hist.tsv
    
    cat $1/temp/*_ss_duplication_rate.tsv | grep "^sample" | head -n 1 > $1/out_dir/all_samples_ss_duplication_rate.tsv
    cat $1/temp/*_ss_duplication_rate.tsv | grep -v "^sample" >> $1/out_dir/all_samples_ss_duplication_rate.tsv
    
    cat $1/temp/*_duplex_single_reads.tsv | grep "^sample" | head -n 1 > $1/out_dir/all_samples_duplex_single_reads.tsv
    cat $1/temp/*_duplex_single_reads.tsv | grep -v "^sample" >> $1/out_dir/all_samples_duplex_single_reads.tsv
   
}

#$1 is properties directory
#prop_dir=/media/e/test_dir/properties

parse_props $prop_dir
        
