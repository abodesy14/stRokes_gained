## Calculate your own Strokes Gained
Welcome to the stRokes_gained repo, a free tool that lets you analyze your game like the pros do.<br>

Try the Shiny app to find the strengths and weaknesses of your own game: https://abodesy14.shinyapps.io/stRokes_gained/

## Why Use Strokes Gained?<br>
"Strokes Gained" is a modern approach to evaluating golfer performance. Rather than tracking traditional stats like fairways hit, greens in regulation, or total putts, Strokes Gained puts every shot in context by comparing it to the average outcome for golfers of specific handicap levels. 2-putting from 60 feet vs 4 feet shouldn't be looked at the same, just as hitting the fairway with a driver vs a pitching wedge shouldn't either.

These averages are derived from millions of shots across different lie/distance/handicap combinations, allowing us to estimate the expected number of strokes from any position on the course. This makes it easier to identify where you're gaining or losing strokes, and where to focus your practice.

## Real-World Example:<br>

<img width="310" alt="15 yards in sand" src="https://github.com/user-attachments/assets/56cc6a11-ff96-49ee-ba0f-020c2c2c7f50" width=45%>

Take for example the 400-yard hole in the image above. The PGA Tour average for a hole of this length is <strong>3.98 strokes</strong>. Imagine you're a PGA Tour pro for a second, playing this hole:

<p><strong>Shot 1</strong>: You tee off from 400 yards, ending up at 100 yards in the fairway. The expected strokes for a PGA Tour pro from 100 yards in the fairway is <strong>2.78</strong>.</p>
<ul>
  <li><em>Calculation:</em> (3.98 - 1) - 2.78 = <strong>+0.20 strokes gained</strong></li>
</ul>

<p><strong>Shot 2</strong>: From 100 yards, your second shot ends up in the sand, 15 yards from the pin. The expected strokes from 15 yards in sand is <strong>2.5</strong>.</p>
<ul>
  <li><em>Calculation:</em> (2.78 - 1) - 2.5 = <strong>-0.72 strokes lost</strong></li>
</ul>

<p><strong>Shot 3</strong>: You hit your bunker shot to 6 feet on the green. The expected strokes from 6 feet is <strong>1.34</strong>.</p>
<ul>
  <li><em>Calculation:</em> (2.5 - 1) - 1.34 = <strong>+0.16 strokes gained</strong></li>
</ul>

<p><strong>Shot 4</strong>: You hole your 6-foot putt.</p>
<ul>
  <li><em>Calculation:</em> 1.34 - 1 = <strong>+0.34 strokes gained</strong></li>
</ul>

In aggregate, you <strong>lost -0.02 strokes</strong> on the hole (4 - 3.98), using PGA Tour averages. Each shot had its own value, some positive, some negative. Over time, this type of analysis can surface trends and uncover the true strengths and weaknesses in your game.<br>

## Using the App
There are 2 states within the app: <strong>guest mode</strong> and <strong>user login</strong>. Guest mode allows you to enter in shots for which the app will calculate your Strokes Gained. Creating a free account allows you to submit rounds to the tool and see 40+ different kpis and game analysis tools. 

Regardless of whether you're in guest mode or logged in, tracking Strokes Gained with the app is simple. All you need to log is the <strong>starting position of each shot</strong> you took, and whether it went in the hole. Use <strong>Enter</strong> or <strong>Tab</strong> to submit shots. <br> 

If you are logged into your account, I also recommend entering in the <strong>Par and Hole for just the first shot of each hole ONLY</strong>. The rest will automatically be calculated and filled down in the backend. These fields are optional, but some of the game analysis calculations especially scoring relative to par and scorecard generation won't work without them.

Think of the table like an Excel spreadsheet:<br>
- In cell A1, enter the starting location of your first shot. For example, type: <strong>400t</strong> (indicating you are on the tee, 400 yards from the pin).
- In cell A2, type in <strong>100f</strong> (100 yards from the pin in the fairway)
- In cell A3, enter in <strong>15s</strong> (15 yards from the pin in the sand)
- Lastly, in cell A4, enter <strong>6g</strong>, and type any value into the 'Ball in Hole' column to indicate it was holed out<br>

<strong>Note</strong>: All distances from off the green are considered to be in <strong>yards</strong>, while distances on the green (putts) should be in <strong>feet</strong>, as that's the common golf convention.<br>

As you log your shots, your Strokes Gained will be calculated in the rightmost column, assuming you've entered a valid distance/lie value. The calculation is responsive to the handicap baseline set at the top. Any row with an invalid distance/lie combination will turn red.

<strong>Screen Recording below</strong>:<br>
https://github.com/user-attachments/assets/4583ee50-6086-4e7c-ba9e-72202a37954a 
<br>

The app defaults to loading in 36 shots. Use the "Append" button to add additional shots as needed. Your results can be exported to csv with the "Download" button. Keep in mind that refreshing the page will likely cause you to lose anything you've logged. Admittedly, the UI for entering shots isn't the greatest, and is not the most mobile friendly either, so I'd recommend using this tool via computer. <br><br>

## Additional Features for Users with Account
If you plan on entering in multiple rounds or want to get a more complete view of your golf game over time, you may want to consider making a free account. Account creation makes the following tabs available to you:<br>
- <strong>Edit Rounds: </strong>For making retroactive edits to previous rounds you've submitted. Maybe you entered in the wrong yardage or club used for a shot and want to fix that. <br>

- <strong>Scorecards: </strong> Renders a scorecard with some extra bells and whistles. It shows GIRs, FIRs, feet of putts made, cumulative score to par, and Strokes Gained across the 4 main categories. Below is an example from one of my recent rounds. <strong>*Note*:</strong> You must plug in "d" or "driver" in the "Club" column for this metric to populate so the tool knows where driver was used. Driving distance is only calculated for shots with driver so that the average is not dragged down by less-than-driver or forced layup tee shots. 
<img width="2942" height="1360" alt="image" src="https://github.com/user-attachments/assets/5fb0f5f2-b709-466f-bd48-4227b9629541" /> <br>

- <strong>KPIs: </strong> Grid of 42 KPIs. All are automatically derived as a product of entering in the start position of each shot. Quite powerful compared to the traditional tracking of GIRs, FIRs, and putts as I alluded to above. Refer to the Data Dictionary tab for definitions for each.
<img width="2978" height="1530" alt="image" src="https://github.com/user-attachments/assets/1f7f5309-4295-445e-aebf-c787776037a2" /> <br>

- <strong>SG by Round: </strong> Table that breaks out Strokes Gained OTT, APP, ARG, PUTT for each round. The percentile of each value is also shown.
<img width="2906" height="464" alt="image" src="https://github.com/user-attachments/assets/fa5b8bf8-4316-42c0-8d7c-ce1123beeee4" /> <br>

- <strong>Moving Avg: </strong>Chart inspired by datagolf https://datagolf.com/player-profiles. Shows the moving average for any of the 4 Strokes Gained categories, or Totals. Green bars represent positive Strokes Gained for that round. Red bars represent negative Strokes Gained for that round. Blue is for rounds that aren't 9 or 18 hole rounds. <img width="2948" height="1244" alt="image" src="https://github.com/user-attachments/assets/52965fb8-0640-49a2-8dc0-743a67f14932" /> <br>

- <strong>Cumulative: </strong>Facet grid of cumulative line charts for each Strokes Gained category.
<img width="2970" height="1014" alt="image" src="https://github.com/user-attachments/assets/89452934-7c46-46e9-95cc-b3ea5ebca279" /> <br>

- <strong>SG by Category: </strong> Strokes Gained per shot by detailed 25 yard increments, or 5 foot increments for putting. Number of shots taken within each bucket is shown on the end of each bar.
<img width="2968" height="1202" alt="image" src="https://github.com/user-attachments/assets/dd696c76-c7df-4939-8756-6decba620a2c" /> <br>

- <strong>Best and Worst Shots: </strong>Defaults to showing your top and bottom 5 shots for any round(s) you select. The shot verbiage column relies on you entering in the Par, Hole, and Club for it to make sense. Change the "Top/Bottom N:" at the top to show more or less than 5.
<img width="2924" height="1492" alt="image" src="https://github.com/user-attachments/assets/7b933992-b40f-4cf9-98c3-271a42b867d9" /> <br>

- <strong>Data Dictionary: </strong>Detailed KPI definitions. <br>

- <strong>Change Password: </strong>Tab for changing your password if logged in. If you forget your password and get locked out of the tool, email me at adam.c.beaudet@gmail.com and I will generate a temp password for you. I have not built in self-serving email password reset capabilities yet, but maybe one day.


## Future Roadmap
- Public leaderboards
- Set default handicap by user
- Possible tool migration for better UI and data entry
- Password reset via email
