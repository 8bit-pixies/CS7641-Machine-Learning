function [Map_Data]=read_data(file_name,m,n)
% read_data   Reads racetrack data from a file and deduces a Map_Data matrix
%             that represents the racetrack
% Arguments --------------------------------------------------------------
%     m = the number of lines of the track matrix;
%     n = the number of rows of the track matrix;
%     file_name = string containing the path to the data file.
% Evaluation -------------------------------------------------------------
%     Map_Data = the matrix that represents the racetrack.
%--------------------------------------------------------------------------
% In verbose mode, there is no difference to silent mode.
%--------------------------------------------------------------------------
% MDP Toolbox, INRA, BIA Toulouse, France
%--------------------------------------------------------------------------

% Reading the data into a mxn matrix;

fid=fopen(file_name,'r');
Map_Data=(fscanf(fid,'%5d',[m n])');
