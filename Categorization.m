% This script runs a categorization experiment, in which listeners hear two
% ends of the continuum (ba,da, sa, or sha) and then a third stimulus. They
% must then click on the appropriate button, categorizing the stimulus.
%
% The layout should be two buttons in the center of the screen. For now,
% they should read 1 and 2. The buttons should change color whenever the
% corresponding sound is being played. Maybe not true buttons- just boxes
% on the screen. 
% New features: kid friendly pictures
% Now using Psychport Audio


function Categorization(varargin)

% Pass in the parameters
if nargin < 1
    SubjectCode = 'nnn';
    test_cue = 'Ba_Da';
    task = 'categorization';
    reps = 1;
    stim_path = './Stimuli';
    results_path = './Results';
    rand_ab = 0;
    a_first = 1;
    b_first = 0;
    single_interval = 0;
    
else
    SubjectCode = varargin{1};
    test_cue = varargin{2};
    task = varargin{3};
    reps = varargin{4};
    stim_path = varargin{5};
    results_path = varargin{6};
    rand_ab = varargin{7};
    a_first = varargin{8};
    b_first = varargin{9};
    single_interval = varargin{10};
end





% Call some default settings
PsychDefaultSetup(2);
InitializePsychSound


% Disable keys except the arrows
RestrictKeysForKbCheck([115, 117])
% Get the screen numbers
screens = Screen('Screens');

% Select the external screen if it is present, else revert to native screen
screenNumber = max(screens);

% Define colors 
background = [83, 58, 113] ./ 255;


% Open an on screen window and color it
[window, ~] = PsychImaging('OpenWindow', screenNumber, background);

% Get the size of the onscreen window in pixels
[screenXpixels, screenYpixels] = Screen('WindowSize', window);




%% Load in sounds
stim_list_dir = './Stim_List';
stim_list_file_base = ['stimlist_' test_cue '.txt'];

list_base = read_list([stim_list_dir '/' stim_list_file_base]);

% get endpoints

% Which continua?
if strcmp(test_cue, 'Ba_Da')
    end_pt_1_label = 'Ba';
    end_pt_2_label = 'Da';
elseif strcmp(test_cue, 'Sa_Sha')
    end_pt_1_label = 'Sha';
    end_pt_2_label = 'Sa';
end

% first extreme
play_file = [stim_path '/' list_base{1}];
[audio, freq] = audioread(play_file);
cont_end_1 = [audio'; audio'];

% second extreme
play_file = [stim_path '/' list_base{7}];
[audio, freq] = audioread(play_file);
cont_end_2 = [audio'; audio'];

% Set interstumulus interval
ISI = 0.4;
ITI = 0.5; % set intertrial interval

num_trials_each = reps; % How many times each stimulus will be presented
shuffled_list = permute_list(list_base, num_trials_each);

%% Load in images
imageLocation = './Images';


if strcmp(test_cue, 'Ba_Da')
    end_pt_1_image_name = 'sheep_1.png';
    end_pt_2_image_name = 'sheep_2.png';
    end_pt_1_image_name_glow = 'sheep_1_glow.png';
    end_pt_2_image_name_glow = 'sheep_2_glow.png';
    
elseif strcmp(test_cue, 'Sa_Sha')
    end_pt_1_image_name = 'snake_1.png';
    end_pt_2_image_name = 'snake_2.png';
    end_pt_1_image_name_glow = 'snake_1_glow.png';
    end_pt_2_image_name_glow = 'snake_2_glow.png';
end

image_1 = imread([imageLocation '/' end_pt_1_image_name], 'BackgroundColor', background);
image_1_glow = imread([imageLocation '/' end_pt_1_image_name_glow], 'BackgroundColor', background);
image_2 = imread([imageLocation '/' end_pt_2_image_name], 'BackgroundColor', background);
image_2_glow = imread([imageLocation '/' end_pt_2_image_name_glow], 'BackgroundColor', background);


% Get size of images
[s11, s21, ~] = size(image_1);
[s12, s22, ~] = size(image_2);

aspect_ratio_1 = s21/s11;
aspect_ratio_2 = s22/s12;

imageHeights = 500;
imageWidth1 = imageHeights .* aspect_ratio_1;
imageWidth2 = imageHeights .* aspect_ratio_2;

imageTexture1 = Screen('MakeTexture', window, image_1);
imageTexture2 = Screen('MakeTexture', window, image_2);
imageTexture1_glow = Screen('MakeTexture', window, image_1_glow);
imageTexture2_glow = Screen('MakeTexture', window, image_2_glow);

% make the destination rectangles for our image

dstRects = zeros(4, 2);
theRect1 = [0 0 imageWidth1 imageHeights];
theRect2 = [0 0 imageWidth2 imageHeights];

dstRects(:,1) = CenterRectOnPointd(theRect1, screenXpixels/4, screenYpixels/2);
dstRects(:,2) = CenterRectOnPointd(theRect2, screenXpixels*(3/4), screenYpixels/2);


%% Set up where to save results
repeat_number = 1;
results_file_base = [SubjectCode '_' task '_' test_cue '_' num2str(repeat_number) '.txt'];
results_file = [results_path '/' results_file_base];

% Check if this file already exists
while exist(results_file) == 2
    
    %update the repeat number and then the file name
    repeat_number = repeat_number + 1;
    results_file_base = [SubjectCode '_' task '_' test_cue '_' num2str(repeat_number) '.txt'];
    results_file = [results_path '/' results_file_base];
end

output_pointer = fopen(results_file, 'w');

data_header_row = 'trial,stimulus,sound1,sound2,selection,RT';
%timestamp = datestr(datetime('now'));
timestamp = fix(clock);
fprintf(output_pointer, '%d-%d-%d,%d:%d:%d\n', timestamp(1),timestamp(2),timestamp(3),timestamp(4),timestamp(5),timestamp(6));
fprintf(output_pointer, '%s\n',data_header_row);
fclose(output_pointer);


%% Load up all the sounds in a buffer

for i = 1:length(shuffled_list)
    % Select at random a sound from the continuum
play_file = [stim_path '/' shuffled_list{i}];
[audio, freq] = audioread(play_file);
test_wavedata{i} = [audio'; audio'];



% Want to know the stimulus step, for plotting our psychometric at the end
tmp_str = strsplit(shuffled_list{i}, {'_','.'});
stimulus_step(i) = str2num(tmp_str{4});
end

% Get a list or random numbers to determine order of stimulus presentation
if rand_ab
    choice_order = randi(2, 1, length(shuffled_list));
elseif a_first
    choice_order = ones(1, length(shuffled_list));
elseif b_first
    choice_order = 2*ones(1,length(shuffled_list));
end

%% Open the default audio device
pahandle = PsychPortAudio('Open', [],[],0,freq,2);

%% Make a vector to store the percent classified as end_pt_1 for plotting at the end
% How many steps are in the continuum?
num_steps_in_continuum = length(list_base); 
psychometric = zeros(1, num_steps_in_continuum);




%% ************************MAIN EXPERIMENT LOOP****************************

if single_interval == 0
    for j = 1:length(shuffled_list)
    %% Starting screen

    % Draw two animals
    Screen('DrawTextures', window, imageTexture1, [], dstRects(:,1));
    Screen('DrawTextures', window, imageTexture2, [], dstRects(:,2))

    % Flip to the screen
    Screen('Flip', window);


    %% Play the first end of the continuum

    % Color the correct button


    % Flip window and start playing sound

    if choice_order(j) == 1
        use_end = cont_end_1;
        choice1 = end_pt_1_label;
        Screen('DrawTextures', window, imageTexture1_glow, [], dstRects(:,1));
        Screen('DrawTextures', window, imageTexture2, [], dstRects(:,2))

    elseif choice_order(j) == 2
        use_end = cont_end_2;
        choice1 = end_pt_2_label;
        Screen('DrawTextures', window, imageTexture1, [], dstRects(:,1));
        Screen('DrawTextures', window, imageTexture2_glow, [], dstRects(:,2))
    end

    sound1_start = Screen('Flip', window);
    PsychPortAudio('FillBuffer', pahandle, use_end);
    PsychPortAudio('Start', pahandle, 1,[]);
    PsychPortAudio('Stop', pahandle, 1);

    WaitSecs(ISI)
    %% Play the second end of the continuum

    if choice_order(j) == 1
        use_end = cont_end_2;
        choice2 = end_pt_2_label;
        Screen('DrawTextures', window, imageTexture1, [], dstRects(:,1));
        Screen('DrawTextures', window, imageTexture2_glow, [], dstRects(:,2))
    elseif choice_order(j) == 2
        use_end = cont_end_1;
        choice2 = end_pt_1_label;
        Screen('DrawTextures', window, imageTexture1_glow, [], dstRects(:,1));
        Screen('DrawTextures', window, imageTexture2, [], dstRects(:,2))
    end


    sound2_start = Screen('Flip', window);
    PsychPortAudio('FillBuffer', pahandle, use_end);
    PsychPortAudio('Start', pahandle, 1,[]);
    PsychPortAudio('Stop', pahandle, 1);
    WaitSecs(ISI)

    %% Return to a neutral screen and play the third sound
    % Draw two animals
    Screen('DrawTextures', window, imageTexture1, [], dstRects(:,1));
    Screen('DrawTextures', window, imageTexture2, [], dstRects(:,2))

    sound3_start = Screen('Flip', window);
    PsychPortAudio('FillBuffer', pahandle, test_wavedata{j});
    PsychPortAudio('Start', pahandle, 1,[]);
    PsychPortAudio('Stop', pahandle, 1);
    reps_start_time = GetSecs;


    keyPress = 0;
    % Wait for a keystroke to terminate
    while keyPress ==0
        [keyPress, secs, keyCode] = KbCheck();

    end

    disp(keyPress)


    Response_time = secs - reps_start_time;

    % Categorize as choice 1 or choice 2
    kbNameResult = KbName(keyCode);
    disp(kbNameResult)
    if strcmp(kbNameResult,'DownArrow')
        selection = end_pt_1_label;

    elseif strcmp(kbNameResult,'RightArrow')
        selection = end_pt_2_label;
         psychometric(stimulus_step(j)) = psychometric(stimulus_step(j))+1;
    else
        selection = 'NA';
    end


    % Write to file
    output_pointer = fopen(results_file, 'a');
    fprintf(output_pointer, '%d,%s,%s,%s,%s,%d\n',...
            j, ... %d
            shuffled_list{j}, ... %s
            choice1, ... %s
            choice2, .... %s,
            selection, ... %s
            Response_time); %d

    % Increment the psychometric function by 
    WaitSecs(ITI)

    end

else
    for j = 1:length(shuffled_list)
    %% Starting screen

    % Draw two animals
    Screen('DrawTextures', window, imageTexture1, [], dstRects(:,1));
    Screen('DrawTextures', window, imageTexture2, [], dstRects(:,2));

    % Flip to the screen
    Screen('Flip', window);
    PsychPortAudio('FillBuffer', pahandle, test_wavedata{j});
    PsychPortAudio('Start', pahandle, 1,[]);
    PsychPortAudio('Stop', pahandle, 1);
    reps_start_time = GetSecs;


    keyPress = 0;
    % Wait for a keystroke to terminate
    while keyPress ==0
        [keyPress, secs, keyCode] = KbCheck();

    end


    Response_time = secs - reps_start_time;

    % Categorize as choice 1 or choice 2
    kbNameResult = KbName(keyCode);
    disp(kbNameResult)
    if strcmp(kbNameResult,'DownArrow')
        selection = end_pt_1_label;

    elseif strcmp(kbNameResult,'RightArrow')
        selection = end_pt_2_label;
         psychometric(stimulus_step(j)) = psychometric(stimulus_step(j))+1;
    else
        selection = 'NA';
    end


    % Write to file
    output_pointer = fopen(results_file, 'a');
    fprintf(output_pointer, '%d,%s,%s,%s,%s,%d\n',...
            j, ... %d
            shuffled_list{j}, ... %s
            'NA', ... %s
            'NA', .... %s,
            selection, ... %s
            Response_time); %d

    % Increment the psychometric function by 
    WaitSecs(ITI);
        
    end

end
% Clear the screen
sca
PsychPortAudio('Close')

% Plot the psychometric for the experimenter
psychometric_viewer(psychometric, reps)

end

