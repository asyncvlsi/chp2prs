testchp:
	@if [ -d test -a -x test/run.sh ] ; \
	then \
		(cd test/; ./run.sh);\
	else \
		(echo "Error: no run.sh file") \
	fi
	
prsimscr:
	@if [ -x ${ACT_HOME}/bin/test_writer -a -d test -a -d test/unit_tests -a -d test/unit_tests/${unit} ] ; \
	then \
		if [ -f test/unit_tests/${unit}/test_writer.txt ] ; \
		then \
			(echo "Creating ${unit} prsim script..."); \
			(${ACT_HOME}/bin/test_writer test/unit_tests/${unit}/test_writer.txt test/unit_tests/${unit}/test.prsim --prsim --reset --_reset); \
		fi \
	else \
		(echo "ERROR: no test writer executable or correct unit folder"); \
	fi
