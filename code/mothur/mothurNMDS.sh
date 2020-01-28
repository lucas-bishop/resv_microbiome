#! /bin/bash
# mothurNMDS.sh
# William L. Close
# Schloss Lab
# University of Michigan

##################
# Set Script Env #
##################

# Set the variables to be used in this script
export DIST=$1

export ACCNOS=$2

export SEED=20170415



###################
# NMDS Ordination #
###################

# Pulling distance type from file name
DISTTYPE=$(echo "${DIST}" | sed 's/.*\.\([a-z]*\)\.0.*/\1/')

# Run diversity analysis on new aligned data set
echo PROGRESS: Calculating NMDS ordination and metrics using "${DISTTYPE}" distance.

# If $ACCNOS isn't provided
if [ -z "${ACCNOS}" ]; then

	# Calculate axes for the whole distance file
	mothur "#nmds(phylip="${DIST}", seed="${SEED}")"

else

	# Use $ACCNOS to remove sequences from the distance file then calculate axes
	mothur "#remove.dists(phylip="${DIST}", accnos="${ACCNOS}");
		nmds(phylip=current, seed="${SEED}")"

	FILEPATH=$(echo "${DIST}" | sed 's;\(.*\/\).*;\1;')
	REMGROUP=$(echo "${ACCNOS}" | sed 's/.*\.final\.\([a-z1-9]*\).*/\1/')

	mv "${FILEPATH}"/*.pick.dist $(echo "${FILEPATH}"/*.pick.dist | sed 's/\.pick\./&'"${REMGROUP}"'\./')

	for FILE in $(ls "${FILEPATH}" | grep "\.pick\.nmds\.[a-z]*$"); do
		NEWNAME=$(echo "${FILE}" | sed 's/\.pick\.nmds\./&'"${REMGROUP}"'\./')
		mv "${FILEPATH}"/"${FILE}" "${FILEPATH}"/"${NEWNAME}"
	done

fi


