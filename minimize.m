%MAXIMIZE  Minimize a figure window
%
% Examples:
%   minimize
%   minimize(hFig)
%
% Minimizes the current or input figure 
%
%IN:
%   hFig - Handle of figure to minimize. Default: gcf.

function minimize(hFig)
if nargin < 1
    hFig = gcf;
end
drawnow % Required to avoid Java errors
jFig = get(handle(hFig), 'JavaFrame'); 
jFig.setMinimized(true);