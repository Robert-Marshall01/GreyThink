function runAllTests()
    %RUNALLTESTS Run all Grey SDK tests
    %   runAllTests() runs all tests and displays results
    
    % Import test framework
    import matlab.unittest.TestSuite
    import matlab.unittest.TestRunner
    import matlab.unittest.plugins.TestReportPlugin
    
    % Get the test directory
    testDir = fileparts(mfilename('fullpath'));
    
    % Add SDK to path
    sdkDir = fileparts(testDir);
    addpath(sdkDir);
    
    % Create test suite from folder
    suite = TestSuite.fromFolder(testDir);
    
    % Create runner with text output
    runner = TestRunner.withTextOutput('Verbosity', 3);
    
    % Run tests
    results = runner.run(suite);
    
    % Display summary
    fprintf('\n=== Test Summary ===\n');
    fprintf('Total:  %d\n', length(results));
    fprintf('Passed: %d\n', sum([results.Passed]));
    fprintf('Failed: %d\n', sum([results.Failed]));
    fprintf('Errors: %d\n', sum([results.Incomplete]));
    
    % Return exit code for CI
    if nargout > 0
        exitCode = ~all([results.Passed]);
    end
end
