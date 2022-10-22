#! /bin/sh
# initialize empty database
# use cat instead of cp to preserve the UMASK
# Return 1 (unsucessful) if any of the cat commands fails.
# (The "exit 1" is executed only if the cat command returns non-zero)
# 
cat /usr/WorkShop/usr/lib/WorkShop/cvdb1.dat > ./cvdb1.dat || exit 1
cat /usr/WorkShop/usr/lib/WorkShop/cvdb2.dat > ./cvdb2.dat || exit 1
cat /usr/WorkShop/usr/lib/WorkShop/cvdb3.dat > ./cvdb3.dat || exit 1
cat /usr/WorkShop/usr/lib/WorkShop/cvdb4.dat > ./cvdb4.dat || exit 1
cat /usr/WorkShop/usr/lib/WorkShop/cvdb5.dat > ./cvdb5.dat || exit 1
cat /usr/WorkShop/usr/lib/WorkShop/cvdb6.dat > ./cvdb6.dat || exit 1
cat /usr/WorkShop/usr/lib/WorkShop/cvdb7.dat > ./cvdb7.dat || exit 1
cat /usr/WorkShop/usr/lib/WorkShop/cvdb8.dat > ./cvdb8.dat || exit 1
cat /usr/WorkShop/usr/lib/WorkShop/cvdb9.dat > ./cvdb9.dat || exit 1
cat /usr/WorkShop/usr/lib/WorkShop/cvdb10.dat > ./cvdb10.dat || exit 1
cat /usr/WorkShop/usr/lib/WorkShop/cvdb11.dat > ./cvdb11.dat || exit 1
cat /usr/WorkShop/usr/lib/WorkShop/cvdb12.dat > ./cvdb12.dat || exit 1
cat /usr/WorkShop/usr/lib/WorkShop/cvdb13.dat > ./cvdb13.dat || exit 1
cat /usr/WorkShop/usr/lib/WorkShop/cvdb14.dat > ./cvdb14.dat || exit 1
cat /usr/WorkShop/usr/lib/WorkShop/cvdb15.dat > ./cvdb15.dat || exit 1
cat /usr/WorkShop/usr/lib/WorkShop/cvdb16.dat > ./cvdb16.dat || exit 1
cat /usr/WorkShop/usr/lib/WorkShop/cvdb.dbd > ./cvdb.dbd || exit 1
cat /usr/WorkShop/usr/lib/WorkShop/cvdb.key > ./cvdb.key || exit 1
cat /usr/WorkShop/usr/lib/WorkShop/cvdb.compound.key > ./cvdb.compound.key || exit 1
rm -f cvstatic.src
# if we fall through, return successfully
exit 0
