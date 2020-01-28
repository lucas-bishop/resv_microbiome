#! /bin/bash
# mothurCountShared.sh
# William L. Close
# Schloss Lab
# University of Michigan

##################
# Set Script Env #
##################

# Set the variables to be used in this script
export WORKDIR=$1



#########################
# Counting Shared Files #
#########################

# Creating list of all group-specific shared files (ex: sample.final.shared, etc.)
SHAREDLIST=$(ls "${WORKDIR}"/ | grep "\.final\.shared$")



# Generating read count tables for shared files
echo PROGRESS: Generating read count tables.

for SHARED in $(echo "${SHAREDLIST}"); do
	mothur "#count.groups(shared="${WORKDIR}"/"${SHARED}")"
done


