#! /bin/bash
# mothurPCoA.sh
# William L. Close
# Schloss Lab
# University of Michigan

##################
# Set Script Env #
##################

# Set the variables to be used in this script
export DIST=$1

export ACCNOS=$2


###################
# PCoA Ordination #
###################

# Pulling distance type from file name
DISTTYPE=$(echo "${DIST}" | sed 's/.*\.\([a-z]*\)\.0.*/\1/')

# Run diversity analysis on new aligned data set
echo PROGRESS: Calculating PCoA ordination and metrics using "${DISTTYPE}" distance.

# If $ACCNOS isn't provided
if [ -z "${ACCNOS}" ]; then

	# Calculate axes for the whole distance file
	mothur "#pcoa(phylip="${DIST}")"

else

	# Use $ACCNOS to remove sequences from the distance file then calculate axes
	mothur "#remove.dists(phylip="${DIST}", accnos="${ACCNOS}");
		pcoa(phylip=current)"

	FILEPATH=$(echo "${DIST}" | sed 's;\(.*\/\).*;\1;')
	REMGROUP=$(echo "${ACCNOS}" | sed 's/.*\.final\.\([a-z1-9]*\).*/\1/')

	mv "${FILEPATH}"/*.pick.dist $(echo "${FILEPATH}"/*.pick.dist | sed 's/\.pick\./&'"${REMGROUP}"'\./')

	for FILE in $(ls "${FILEPATH}" | grep "\.pick\.pcoa\.[a-z]*$"); do
		NEWNAME=$(echo "${FILE}" | sed 's/\.pick\.pcoa\./&'"${REMGROUP}"'\./')
		mv "${FILEPATH}"/"${FILE}" "${FILEPATH}"/"${NEWNAME}"
	done

fi


