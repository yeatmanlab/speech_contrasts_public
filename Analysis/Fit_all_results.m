% This scripts fits all the subjects to psychometric functions, based on
% the contents of a subject list.

close all
clear all

% Get the subject list from the directory of results
file_list = dir('../Results/Psychometrics/Raw');

sid_list = {};

for i = 3:length(file_list)
    fname = file_list(i).name;
    s = strsplit(fname, '_');
    sid_to_split = s{4};
    sid_split = strsplit(sid_to_split,'.');
    sid = sid_split{1};
    
    sid_list{i-2} = sid;
end

% Just take the unique values of the sid list
sid_list = unique(sid_list);
continuum = 'Sha_Sa'; % Choices: 'Sha_Sa' or 'Ba_Da'


% For each, compute the fit
for i = 1:length(sid_list)
    
    % Inputs: subject id, continuum, width of rectangular prior
    read_psychometric_and_fit(sid_list{i},continuum,0.1)
       
    
end

