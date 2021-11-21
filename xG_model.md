# NHL Expected Goals (xG) Model

This model is calculating 'expected goals', ie the likelihood that a particular shot in an NHL game will turn into a goal.  There are many such models out there of varying complexity, often using proprietary data like pre-shot movement.  This one is faily simple by comparison, using only publicly available data (see NHL API script) and illustrative of how one could construct a more robust model.  It is trained on NHL shot data from 2011 to 2020, and is using a handful of features including shot type, shot distance, shot angle, and time since last shot (rebound).  'Expected Goals' can be used as a better indication of how many quality scoring chances a team creates during a game, rather than using more traditional metrics like shots on goal, corsi or fenwick shots.

### Data Preprocessing

First we define which features we are going to use:

`predictor_names <- c("secondaryType",
                     "dist",
                     "angle",
                     "last_shot")`
                     
Next, we will load some previously saved data from the NHL API which includes all the play by play data from 2011 to 2020:

```
play_data <- c("plays_2011.rds",
               "plays_2012.rds",
               "plays_2013.rds",
               "plays_2014.rds",
               "plays_2015.rds",
               "plays_2016.rds",
               "plays_2017.rds",
               "plays_2018.rds",
               "plays_2019.rds",
               "plays_2020.rds")
```

One we have the data, we need to do some pre-processing on it.  First we filter to only shot and goal events to train our model.  Unfortunately we can't use blocked shots, since it notes the location of the shot block, not the original block.  Also we are not able to use missed shots, since frustratingly the NHL data doesn't include the shot type for a missed shot, which is a key feature of our model.
We also exclude all period == 5 events, as those are shootout events.
Next we calculate shot distance and angle from the x and y location of each shot.  Note that centre ice is 0,0 and the goal line is at x=89.  For shots that recorded at x>89, ie behind the net, we change those to goal line shots at 89ft.  Y locations are + and - for left and right wing, we use a net width of 6 ft (+/- 3 ft) from centre, and calculate the angle from the closest edge of the net.
Finally, we create a helper variable of time in seconds from period start and then calculate the delay from the previous shot.  The assumption is that shots in short duration, ie rebounds, will result in a higher xG probability.

### WOE (Weight of Evidence) Transformation

For this model we then do a WOE transformation on each feature, the method is described in detail here: [https://www.listendata.com/2015/03/weight-of-evidence-woe-and-information.html](https://www.listendata.com/2015/03/weight-of-evidence-woe-and-information.html).  Some benefits of WOE is that nicely handles all our categorical values with needing any dummy variables, it will deal with missing values seamlessly, the the WOE and IV (Information Value) scores will give us some indication of feature importance.

` library(Information) # WOE and IV functions `

` woe_table <- create_infotables(data = xg_data, y="Event") `

This creates our WOE tables for each feature, and then I use a for loop to extract all the woe tables and transform each variable based on it's value, and create new transformed columns with a "woe" suffix and select only those for our training.  Below are the information values (IV) for each feature, looks like all are significant and the most important variable is shot distance

![alt text](image.jpg)
