make
echo "Running parser tests..."
./p4cherry-test -parse > status/test-parse.log 2> status/error
echo "Running desugar tests..."
./p4cherry-test -desugar > status/test-desugar.log 2> status/error
echo "Running instantiation tests..."
./p4cherry-test -instantiate > status/test-instantiate.log 2> status/error
