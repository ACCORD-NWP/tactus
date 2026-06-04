#!/usr/bin/env bash

#this script creates symbolic links to the reference data for the different test cases in the reference checker tests.
SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

if [ ${PWD} != ${SCRIPT_DIR} ]; then
    echo "Please run this script from the directory it is located in: ${SCRIPT_DIR}"
    exit 1
fi

#uncomment the following line to remove all symbolic links before creating new ones

find -type l -delete

export REF_DIR=./CY49t2_AROME_nwp_check_identical_20250209

ln -s $REF_DIR/2025/02/09/00/mbr000/NODE.001_01 NODE.001_01
ln -s $REF_DIR CY49t2_AROME_nwp_check_diff_20250209
ln -s $REF_DIR CY49t2_AROME_nwp_check_generate_nofile_20250209
ln -s $REF_DIR CY49t2_AROME_nwp_check_identical_generate_20250209
ln -s $REF_DIR CY49t2_AROME_nwp_check_smalldiff_20250209
ln -s $REF_DIR CY49t2_AROME_nwp_check_diff_suppress_exception_20250209
