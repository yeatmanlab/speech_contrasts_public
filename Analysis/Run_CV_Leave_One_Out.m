close all
clear

% Parameters of subject
continuum = 'Ba_Da'; % 'Ba_Da' or 'Sha_Sa'
width_vec = 0:0.05:0.5;

% Get the subject list from the directory of results
file_list = dir('../Speech_contrasts/Results/Psychometrics/Fit');

sid_list = {};

for i = 3:length(file_list)
    fname = file_list(i).name;
    s = strsplit(fname, '_');
    sid_to_split = s{4};
    sid_split = strsplit(sid_to_split,'.');
    sid = sid_split{1};
    
    sid_list{i-2} = sid;
end
sid_list_tmp = unique(sid_list);


parfor j = 1:length(sid_list)
    subject_id = sid_list{j};
    % Loop over all possible widths
    for w = 1:length(width_vec)
        
        out1  = Cross_validation_kminus1(subject_id, continuum, 1, width_vec(w));
        out2  = Cross_validation_kminus1(subject_id, continuum, 2, width_vec(w));
        % NOTE: if you change to 'sha-sa', use function
        % Cross_validation_kminus1_shasa() instead
        % Write to file
        header = 'SubjectID,block,width,p';
        fid = fopen(['/home/eobrien/bde/Projects/Speech_contrasts/Results/CV_Leave_Out/CV_'...
            continuum, '_', subject_id, '_', num2str(width_vec(w)),'.csv'], 'w');
        
        fprintf(fid, '%s\n', header);
        for k = 1:length(out1)
            fprintf(fid, '%s,%f,%f,%f\n', subject_id, 1, width_vec(w), out1(k));
            fprintf(fid, '%s,%f,%f,%f\n', subject_id, 2, width_vec(w), out2(k));
        end
        fclose(fid);
        
        
    end
end
