% Grey SDK Startup Script
% Run this script to add the Grey SDK to your MATLAB path

% Get the directory containing this script
thisDir = fileparts(mfilename('fullpath'));

% Add the SDK to the path
addpath(thisDir);

% Display confirmation
disp('Grey SDK for MATLAB loaded successfully.');
disp('Use: import grey.* to access the SDK');
