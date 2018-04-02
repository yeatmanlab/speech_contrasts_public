% This function returns the goodness-of-fit for repeated cross-validation
function [trial_likelihood] = Cross_validation_kminus1(subject_id, continuum, block, width)


% Add psignifit toolbox path
addpath('/usr/local/MATLAB/R2017a/toolbox/psignifit')

% Read in the raw data for this subject
filename = ['../Results/Raw/', subject_id, '_categorization_', continuum, '_', num2str(block), '.txt' ];
T = readtable(filename);

% Get the psychometric values after removing half of the data
nrow = 1:70;
trial_likelihood = zeros(1, length(nrow));


for i = 1:length(nrow)
   
    % Get the data sans the leaveout point
    T_tmp = T;
    T_tmp([i],:) = [];
    
    obsv = T(i,:);

% Get the percent correct at each step in the training set
    for this_step = 1:7
        step = T_tmp(ismember(T_tmp.stimulus, ['Ba_Da_Step_' ,num2str(this_step), '.wav']),:);
        sel = table2array(step(:,5))';
        idx = strfind(sel, 'Da');
        idx = find(not(cellfun('isempty', idx))); %#ok<STRCL1>
        nCorr_T1(this_step) = length(idx);
        nPres_T1(this_step) = length(sel);
    end


    A_data_T1 = [1:7; nCorr_T1; nPres_T1]';


    %% Fit the psychometric function
    % Options for fitting
    options = struct;
    options.expType = 'YesNo';
    options.sigmoidName = 'logistic';
    options.useGPU = 1;

    % Set the width slope parameters correctly

    % minimum = minimal difference of two stimulus levels
    widthmin = 1;
    % We use the same prior as we previously used... e.g. we use the factor by
    % which they differ for the cumulative normal function
    Cfactor   = (my_norminv(.95,0,1) - my_norminv(.05,0,1))./( my_norminv(1-0.05,0,1) - my_norminv(0.05,0,1));
    % add a cosine devline over 2 times the spread of the data
    options.priors{2} = @(x) ((x.*Cfactor)>=widthmin).*((x.*Cfactor)<=2*widthmin).*(.5-.5*cos(pi.*((x.*Cfactor)-widthmin)./widthmin))...
        + ((x.*Cfactor)>2*widthmin).*((x.*Cfactor)<= 40);

    if width == 0
        options.fixedPars = NaN(5,1);
        options.fixedPars(3) = 0;
        options.fixedPars(4) = 0;
    else
        priorLambda = @(x) (x>=0).*(x<=width);
        options.priors{3} = priorLambda;
        options.priors{4} = priorLambda;
    end

    results = psignifit(A_data_T1, options);
    %% Assess the deviance on the other half of the dataset
    x = results.data(:,1);
    fit_values = (1-results.Fit(3)-results.Fit(4))*arrayfun(@(x) results.options.sigmoidHandle(x,results.Fit(1),results.Fit(2)),x)+results.Fit(4);
    
    % Did we choose "ba" or "da" on the observation case? 
    obsv_stimulus = regexp(obsv.stimulus{1},'\d*','Match');
    obsv_step = str2double(obsv_stimulus{1});
    
    % Did they get it right?
    if strcmp(obsv.selection{1}, 'Da') %If they said 'da'
        p = fit_values(obsv_step);
    else
        p = 1 - fit_values(obsv_step);
    end

     trial_likelihood(i) = p;
end
