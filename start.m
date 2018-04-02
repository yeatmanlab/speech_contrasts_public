% This is the start screen for the categorization experiment. It should
% give...
%   choice of user ID
%   choice of results file location
%   choice of stimulus location
%   choice of stimulus continuum (ba-da, sa-sha)
%   choice of experiment type (categorization or discrimination)
%   choice of # of reps

function start()

close all


blue1 = [97, 132, 216] ./ 255;
blue2 = [83, 58, 113] ./ 255;
green1 = [80, 197, 183] ./ 255;
green2 = [156, 236, 91] ./ 255;
yellow = [240, 244, 101] ./ 255;


hndl.StartupWindow = figure('name', 'Experiment_Setup', ...
    'numbertitle','off','menubar','none',...
    'color', green1, ...
    'units','normalized','position',[0 0 1 1]);

% Big text on screen
uicontrol('style','text','units','normalized','position',[.35 .79 .35 .2], ...
    'backgroundcolor', green1,'string', 'Speech Contrasts', 'fontsize', 50,...
    'FontWeight','bold')

%% Listener code
% Listener code box label
uicontrol('style', 'text', 'units','normalized','position',[.02 .65 .25 .16],...
    'backgroundcolor',green1,'string','Listener Code:', 'fontsize', 34, 'FontWeight','bold')

% Listener code entry box
hndl.ListenerEntry = uicontrol('style','edit','units','normalized','position', ...
    [.25 .73 .18 .1], 'backgroundcolor', [.97 .97 .97], 'string', 'nnn',...
    'fontsize',34, 'FontWeight','bold');

%% Experiment choices

% Which contrast? Ba-da, or sa-sha
uicontrol('style', 'text', 'units','normalized','position',[.05 .5 .25 .16],...
    'backgroundcolor',green1,'string','Contrast:', 'fontsize', 34, 'FontWeight','bold')

hndl.test_cueEntry = uicontrol('style','popupmenu', ...
    'units','normalized','Position',[.25 .51 .18 .15],...
    'string', ...
    {'Ba_Da';...
    'Sa_Sha'},...
    'FontSize',34);

% Categorization versus discrimination
uicontrol('style', 'text', 'units','normalized','position',[.075 .35 .25 .16],...
    'backgroundcolor',green1,'string','Task:', 'fontsize', 34, 'FontWeight','bold')

hndl.test_type = uicontrol('style','popupmenu', ...
    'units','normalized','Position',[.25 .36 .18 .15],...
    'string', ...
    {'categorization';...
    'discrimination'},...
    'FontSize',30);

% Number of presentations
uicontrol('style', 'text', 'units','normalized','position',[.03 .25 .22 .16],...
    'backgroundcolor',green1,'string','# of presentations per stimulus:', 'fontsize', 28, 'FontWeight','bold')


hndl.reps = uicontrol('style','edit','units','normalized','position', ...
    [.25 .31 .18 .1], 'backgroundcolor', [.97 .97 .97], 'string', '10',...
    'fontsize',34, 'FontWeight','bold');

% Random A B order or not


%uicontrol('style', 'text', 'units','normalized','position',[.03 .1 .22 .16],...
%    'backgroundcolor',green1,'string','Random A-B:', 'fontsize', 28, 'FontWeight','bold')

hndl.randAB = uicontrol('Style','checkbox','Value', 0, 'units', 'normalized',...
    'position', [.05 .2 .2 .1], 'backgroundcolor', green1, ...
    'String','Random A-B','fontsize', 24);


hndl.A_first = uicontrol('Style','checkbox','Value', 0, 'units', 'normalized',...
    'position', [.2 .2 .2 .1], 'backgroundcolor', green1, ...
    'String','A first','fontsize', 24);


hndl.B_first = uicontrol('Style','checkbox','Value', 0, 'units', 'normalized',...
    'position', [.3 .2 .2 .1], 'backgroundcolor', green1, ...
    'String','B first','fontsize', 24);

hndl.single_interval = uicontrol('Style','checkbox','Value', 0, 'units', 'normalized',...
    'position', [.4 .2 .2 .1], 'backgroundcolor', green1, ...
    'String','Single Interval','fontsize', 24);
%% File system parameters

basedir = fileparts(which('start'));

uicontrol('style', 'text', 'units','normalized','position',[.45 .67 .15 .16],...
    'backgroundcolor',green1,'string','Stimulus path:', 'fontsize', 34, 'FontWeight','bold')

hndl.StimPath = uicontrol('style','edit','units','normalized','position', ...
    [.6 .73 .35 .1], 'backgroundcolor', [.97 .97 .97], 'string', ...
    fullfile(basedir,'Stimuli'),...
    'fontsize',14, 'FontWeight','bold');

uicontrol('style', 'text', 'units','normalized','position',[.45 .53 .15 .16],...
    'backgroundcolor',green1,'string','Results path:', 'fontsize', 34, 'FontWeight','bold')

hndl.ResultsPath = uicontrol('style','edit','units','normalized','position', ...
    [.6 .58 .35 .1], 'backgroundcolor', [.97 .97 .97], 'string', ...
    fullfile(basedir,'Results'),...
    'fontsize',14, 'FontWeight','bold');
%% Practice buttons

uicontrol('style','frame', 'units','normalized','position',[.53 .4 .4 .12],...
    'backgroundcolor', blue1);

hndl.practice = uicontrol('style','checkbox',...
    'value', 0, ...
    'units','normalized',...
    'position', [.66 .41 .2 .1],...
    'string','Practice',...
    'backgroundcolor',blue1,...
    'fontsize',36, 'FontWeight','bold');




%% Start button
hndl.StartExp = uicontrol('style','pushbutton',...
    'units','normalized',...
    'position', [.4 .05 .2 .14],...
    'string','Start',...
    'backgroundcolor',yellow,...
    'callback', @SuperStart, 'fontsize',44);

set(findobj('name','Experiment_Setup'), 'UserData', hndl);


end


%% Start the experiment
function SuperStart(varargin)

hndl = get(findobj('name','Experiment_Setup'),'UserData');

% Get the values from user input to set up parameters
SubjectCode = get(hndl.ListenerEntry, 'string');

% ba-da or sa-sha?
list_of_test_cues = get(hndl.test_cueEntry,'string');
test_cue = list_of_test_cues{get(hndl.test_cueEntry, 'value')};

% type of test
list_of_tasks = get(hndl.test_type, 'string');
task = list_of_tasks{get(hndl.test_type, 'value')};

% Get number of reps
reps = str2num(get(hndl.reps,'string'));

% Get the location of the stimulus directory
stim_path = get(hndl.StimPath, 'string');
results_path = get(hndl.ResultsPath, 'string');

% Get stimulus presentation rules
rand_ab = get(hndl.randAB, 'value');
a_first = get(hndl.A_first, 'value');
b_first = get(hndl.B_first, 'value');
single_interval = get(hndl.single_interval, 'value');

% Is this practice?
is_practice = get(hndl.practice, 'value');

minimize(hndl.StartupWindow);

% Throw an error if the user has selected conflicting settings for the
% categorization task.
if sum([rand_ab a_first b_first]) > 1
    error('Can only selet one: Random A-B, A first, or B first.')
end

% Is it practice?
if is_practice && strcmp(task, 'categorization')
    Categorization_Practice(SubjectCode, test_cue, stim_path, results_path, single_interval)
elseif is_practice && strcmp(task, 'discrimination')
    Discrimination_Practice(SubjectCode, test_cue, stim_path, results_path)
elseif ~is_practice && strcmp(task, 'categorization')
    Categorization(SubjectCode,test_cue,task,reps,stim_path,results_path,rand_ab,a_first,b_first,single_interval)
elseif ~is_practice && strcmp(task, 'discrimination')
    Discrimination(SubjectCode, test_cue, task, reps, stim_path, results_path)
end



end