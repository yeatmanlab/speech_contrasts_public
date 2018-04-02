function read_psychometric_and_fit(subject_id, continuum,width)
% arguments%%%%%
%
% subject_id (str)
% continuum (str)

    addpath('/usr/local/MATLAB/R2017a/toolbox/psignifit')
    % Read in table
    filename = ['../Results/Psychometrics/Raw/Psychometrics_', continuum, '_', subject_id, '.csv'];
    T = readtable(filename);

    if strcmp(continuum, 'Sha_Sa')
        endpt1 = 'Sa';
        endpt2 = 'Sha';
    else
        endpt1 = 'Ba';
        endpt2 = 'Da';
    end

    A_psych = T(ismember(T.sound2,{endpt1}),:);
    B_psych = T(ismember(T.sound2,{endpt2}),:);
    C_psych = T(ismember(T.sound2,{'Single'}),:);

    levels = [1:7];
    presentations = 10*ones(1,7);

    %% Get the slope of the first psychometric
    labelled = table2array(A_psych(:,4))' * 10;
    A_data = [levels; labelled; presentations]';

    % Options for fitting
    options = struct;
    options.expType = 'YesNo';
    options.sigmoidName = 'logistic';
    options.useGPU = 1;
    
    % Set the width slope parameters correctly

 % minimum = minimal difference of two stimulus levels
 widthmin = 1;
 % maximum = spread of the data
 xspread = 6;
 widthmax  = xspread;
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
    
    results = psignifit(A_data, options);
    [~,Adev] = getDeviance(results);
    paramsA = getStandardParameters(results);

    % Get fit residuals
    x = results.data(:,1);
    fit_values = (1-results.Fit(3)-results.Fit(4))*arrayfun(@(x) results.options.sigmoidHandle(x,results.Fit(1),results.Fit(2)),x)+results.Fit(4);
    obs_values = results.data(:,2)./results.data(:,3);
    residualsA = sum((fit_values - obs_values).^2);

    % Get model likelihood
    pPred = results.psiHandle(results.data(:,1));
    pMeasured = results.data(:,2)./results.data(:,3);
    loglikelihoodMeasuredA = results.data(:,2).*log(pMeasured)+(results.data(:,3)-results.data(:,2)).*log((1-pMeasured));
    loglikelihoodMeasuredA(pMeasured==1) = 0;
    loglikelihoodMeasuredA(pMeasured==0) = 0;
    loglikelihoodA = sum(abs(loglikelihoodMeasuredA));
    %% Get the slope of the 2nd psychometric
    labelled = table2array(B_psych(:,4))' * 10;
    B_data = [levels; labelled; presentations]';

    % Options for fitting
    options = struct;
    options.expType = 'YesNo';
    options.sigmoidName = 'logistic';
    options.useGPU = 1;
    % Set the width slope parameters correctly

 % minimum = minimal difference of two stimulus levels
 widthmin = 1;
 % maximum = spread of the data
 xspread = 6;
 widthmax  = xspread;
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
    
    
    results = psignifit(B_data, options);
    [~,Bdev] = getDeviance(results);
    paramsB = getStandardParameters(results);
    
    % Get fit residuals
    x = results.data(:,1);
    fit_values = (1-results.Fit(3)-results.Fit(4))*arrayfun(@(x) results.options.sigmoidHandle(x,results.Fit(1),results.Fit(2)),x)+results.Fit(4);
    obs_values = results.data(:,2)./results.data(:,3);
    residualsB = sum((fit_values - obs_values).^2);
    
    
    % Get model likelihood
    pPred = results.psiHandle(results.data(:,1));
    pMeasured = results.data(:,2)./results.data(:,3);
    loglikelihoodMeasuredB = results.data(:,2).*log(pMeasured)+(results.data(:,3)-results.data(:,2)).*log((1-pMeasured));
    loglikelihoodMeasuredB(pMeasured==1) = 0;
    loglikelihoodMeasuredB(pMeasured==0) = 0;
    loglikelihoodB = sum(abs(loglikelihoodMeasuredB));
    %% Get the slope of the 3rd psychometric
    labelled = table2array(C_psych(:,4))' * 10;
    C_data = [levels; labelled; presentations]';

    % Options for fitting
    options = struct;
    options.expType = 'YesNo';
    options.sigmoidName = 'logistic';
    options.useGPU = 1;
    % Set the width slope parameters correctly

 % minimum = minimal difference of two stimulus levels
 widthmin = 1;
 % maximum = spread of the data
 xspread = 6;
 widthmax  = xspread;
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
    
    
    results = psignifit(C_data, options);
    [~,Cdev] = getDeviance(results);
    paramsC = getStandardParameters(results);
        % Get fit residuals
    x = results.data(:,1);
    fit_values = (1-results.Fit(3)-results.Fit(4))*arrayfun(@(x) results.options.sigmoidHandle(x,results.Fit(1),results.Fit(2)),x)+results.Fit(4);
    obs_values = results.data(:,2)./results.data(:,3);
    residualsC = sum((fit_values - obs_values).^2);
    
    % Get model likelihood
    pPred = results.psiHandle(results.data(:,1));
    pMeasured = results.data(:,2)./results.data(:,3);
    loglikelihoodMeasuredC = results.data(:,2).*log(pMeasured)+(results.data(:,3)-results.data(:,2)).*log((1-pMeasured));
    loglikelihoodMeasuredC(pMeasured==1) = 0;
    loglikelihoodMeasuredC(pMeasured==0) = 0;
    loglikelihoodC = sum(abs(loglikelihoodMeasuredC));
    %% Write results to a CSV
    header = 'SubjectID,width,sound2,threshold,slope,lapse,guess,eta,deviance,residuals,loglikelihood';
    fid = fopen(['/home/eobrien/bde/Projects/Speech_contrasts/Results/Psychometrics/Fit_Flat_015/Fit_', ...
        continuum, '_', subject_id, '_', num2str(width),'.csv'], 'w');

    fprintf(fid, '%s\n', header);
    fprintf(fid, '%s,%f,%s,%f,%f,%f,%f,%f,%f,%f,%f\n', subject_id,width,endpt1, paramsA(1),paramsA(2),paramsA(3),paramsA(4),paramsA(5),Adev,residualsA,loglikelihoodA);
    fprintf(fid, '%s,%f,%s,%f,%f,%f,%f,%f,%f,%f,%f\n', subject_id,width, endpt2, paramsB(1),paramsB(2),paramsB(3),paramsB(4),paramsB(5),Bdev,residualsB,loglikelihoodB);
    fprintf(fid, '%s,%f,%s,%f,%f,%f,%f,%f,%f,%f,%f\n', subject_id,width,'Single', paramsC(1),paramsC(2),paramsC(3),paramsC(4),paramsC(5),Cdev,residualsC,loglikelihoodC);

    
end
