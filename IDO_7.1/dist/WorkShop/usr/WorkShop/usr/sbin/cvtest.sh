#! /bin/sh
case $# in
0) echo 'Usage: cvtest.sh <fileset> <logfile>'; exit 2
esac
echo "Cvstatic Testing .. "
echo "	Fileset 	:" $1
echo "	Output File 	:" $2
echo "Building the database ..."
/usr/sbin/cvstatic -batch -fileset $1

# Start up cvstatic iconified
echo "Waiting for cvstatic to initialize ..."
/usr/sbin/cvstatic -iconic &
# This number may have to be bumped up if cvstatic is slow in comming up
sleep 20

echo "Collecting Testing Data ..."
echo `date` >> $2
echo "Fileset :" $1 >> $2
echo "-----------------------------------------------------------" >> $2
/usr/sbin/cvquery AnalyzeFileset $1
/usr/sbin/cvquery FindGlobalDefs >> $2
/usr/sbin/cvquery FindAllMacros >> $2
/usr/sbin/cvquery ListAllVariable >> $2
/usr/sbin/cvquery ListAllFunction >> $2
/usr/sbin/cvquery ListDirectories >> $2
/usr/sbin/cvquery ListAllTypes >> $2
/usr/sbin/cvquery ListAllFile >> $2
/usr/sbin/cvquery ListAllClasses -l >> $2
/usr/sbin/cvquery ListAllCommonBlocks -l >> $2

echo "Test Data Collection Completed"
# echo "Test Data written to" $2 

# Bring down cvstatic
/usr/sbin/cvquery  Quit

