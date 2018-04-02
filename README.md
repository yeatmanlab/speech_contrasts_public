To run this, you will need a computer with MATLAB and Psychtoolbox installed.
Download the entire repository anywhere on your drive. To run, simply open MATLAB, navigate to the project directory, and write "start" in the console. This will give you a start menu where you enter relevant parameters of the experiment. It should be self-explanatory. Be sure to enter the correct filepaths for the stimulus directory and results file directory in the specified fields; they default to give the entries for Elle's lab computer and should be corrected to reflect the file system of the computer running the test. Do not put a trailing slash at the end of the file paths or MATLAB will throw an error.

It takes about 4 minutes to run the categorization experiment with 10 presentations of each stimulus if you're going reasonably fast. If for any reason you want to cancel the experiment midway through, Psychtoolbox can be finnicky with taking control of your monitor and not letting go. In Linux, you can usually hit Ctrl+0 and then type "sca" (return) to close the psychtoolbox test window. Ctrl+C works to cancel the script, but sometimes you have to hit Ctrl+0 first. Again, psychtoolbox is a little funky.

Right now the categorization experiment works but at the time of writing this README there is no discrimination experiment. You can select the discrimination experiment if you want in the start menu, but it would just run the categorization test. 


If you do happen to run this... send me your results file! I'm working up the analysis script right now.
