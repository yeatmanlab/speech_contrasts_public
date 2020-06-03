
FULL DETAILS OF OUR EXPERIMENT ARE AVAILABLE IN OUR PUBLISHED MANUSCRIPT:
https://www.nature.com/articles/s41598-018-34823-8

To run the psychophysical experiment, you will need a computer with MATLAB and Psychtoolbox installed. 
Download the entire repository anywhere on your drive. 

To run, simply open MATLAB, navigate to the project directory, and type "start". 

This will give you a start menu where you enter relevant parameters of the experiment. Be sure to enter the correct filepaths for the stimulus directory and results file directory in the specified fields; they default to give the entries for our lab computer and should be corrected to reflect the file system of the computer running the test. Do not use trailing slashes.

It takes about 4 minutes to run the categorization experiment with 10 presentations of each stimulus if you're going reasonably fast. If for any reason you want to cancel the experiment midway through, Psychtoolbox can be finnicky with taking control of your monitor and not letting go. In Linux, you can usually hit Ctrl+0 and then type "sca" (return) to close the psychtoolbox test window. Ctrl+C works to cancel the script, but sometimes you have to hit Ctrl+0 first. 

All of the analysis scripts are given in the ./Analysis folder. They are written in R. Figure generation scripts are in a subfolder of ./Analysis. To fit your own psychometric functions as we did, you will need to have Psignifit 4 installed.

Raw behavioral data files and fitted pyschometric function parameters for each subject are available in ./Results. Processed behavioral data in longform ready for analysis is available in cleaned_data.csv in the home directory.

