% This script shows a psychometric plot
% Input: 
% -psychometric, a 1 x n vector where n is the number of steps in the psychometric.
% Element(i) indicates how many times stimulus 1 was presented and end_pt_2
% of the continuum was selected.
% -ntrials, the total number of presentations of each stimulus
function psychometric_viewer(psychometric, reps)

psychometric_normalized = psychometric ./ reps; % convert raw score to percent

steps = 1:length(psychometric_normalized);
y_min = 0;
y_max = 1;

hFig = figure;
plot(psychometric_normalized, '-o','LineWidth',4) 

% Make it pretty
box off
ylim([y_min y_max])
set(gca, 'XTick', steps)
xlabel('Step', 'FontSize', 24)
ylabel('% categorized as 2nd endpoint', 'FontSize', 16)


end