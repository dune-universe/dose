
all:
	$(CXX) $(CPPFLAGS) $(LDFLAGS) -lapt-pkg -fPIC -shared -o libcollate_debian.so debian_collation.cpp
	cd sqlitext && make
	cp sqlitext/sqlitext.py sqlitext/_sqlitext.so .

clean:
	rm -f *.pyc
	rm -f libcollate_debian.so
	cd sqlitext && make clean
	rm -f sqlitext.py _sqlitext.so
