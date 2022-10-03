# Data for Stewart, Hermens, and Matthews (2016)

Stewart, N., Hermens, F., & Matthews, W. J. (2016). Eye movements in risky choice. Journal of Behavioral Decision Making, 29, 116-136. [doi: 10.1002/bdm.1854](http://dx.doi.org/10.1002/bdm.1854)

Note: Gamble px offers a smaller probability of a larger amount and gamble
qy offers a larger probability of a smaller amount. Thus px is always the
riskier gamble.

sub Subject number, the name of the directory the raw.asc file is in.

trial Trial number, from 1, gives the order in which trials were presented

expt This is Experiment 5

fixation The number of the fixation within the trial, numbering from when
the fixation cross appears at the start of the trial.

duration The duration of the fixation, in ms.

fx The horizontal position of the fixation, in pixels.

fy The vertical position of the fixation, in pixels.

NB: (0,0) is the top left of the screen.

start The time of the start of the fixation, relative to the onset of the
choice.

end The time of the end of the fixation, relative to the onset of the
choice.

id The ID of the choice, which matches the row in non_dominated_choices.csv

orientation Gambles either read horizontally or vertically

first.attribute Do probabilities or amounts appear first

first.gamble Does the risky or the same gamble appear first (on the top
for horizontal or on the left for vertical)

x The larger amount, in pounds.

p The smaller probability, in %.

y The smaller amount, in pounds.

q The larger probability, in %.

choice Either "px" or "qy". "px" is the riskier choice. Originally choice
records the key pressed in the all_trials_and_responses.csv files.

rt The time taken to press a key, from the onset of the choice.

region The ROI the current fixation falls into. Labels are "p", "x", "q",
or "y", or " " for a non-ROI fixation.

prev.region The region of the immediately preceding fixation.

transition A string with the previous then current fixation labels,
separated by "_"

type Categorises the timing of the fixation, and can be "fixation" for
fixations beginning before the choice onset, "choosing" for fixations
beginning after the choice onset, "choice" for the fixation during the key
press and "post" for fixations beginning after the response.

The .c columns relabel everything  so that px denotes the chosen option and
qy denotes the not-chosen option.

NOTE: Checked two trials from subjects from each cell in the between
subjects design to make sure p, x, q, and y are in the right places and that
the sequence of fixations in region matches up with the replay of the trial
in the EDF viewer.


