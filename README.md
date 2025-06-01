# stRokes_gained

## Calculate Your Own Strokes Gained
Welcome to the stRokes_gained repo - a free tool that lets you analyze your game like the pros do.<br>

 Try the Shiny app to see how your game stacks up: https://abodesy14.shinyapps.io/stRokes_gained/

## Why Use Strokes Gained?<br>
"Strokes Gained" is a modern approach to evaluating golfer performance. Rather than tracking traditional stats like fairways hit, greens in regulation, or total putts, strokes gained puts each shot in context by comparing it to the average outcome for golfers of specific handicap levels.

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

In aggregate, you <strong>lost -0.02 strokes</strong> on the hole (4 - 3.98). Each shot had its own value — some positive, some negative. Over time, this type of analysis can surface trends and uncover the true strengths and weaknesses in your game.<br>

## Using the App
Tracking Strokes Gained with the app is simple - all you need to log is the <strong>starting position of each shot</strong> you took, and whether it went in the hole. Use <strong>Enter</strong> or <strong>Tab</strong> to submit shots. <br>

Think of the table like an Excel spreadsheet:<br>
- In cell A1, enter the starting location of your first shot. For example, type: <strong>400t</strong> (indicating you are on the tee, 400 yards from the pin).
- In cell A2, type in <strong>100f</strong>
- In cell A3, enter in <strong>15s</strong>
- Lastly, in cell A4, enter <strong>6g</strong>, and type any value into the 'Ball in Hole' column to indicate it was holed out<br>

<strong>Note</strong>: All distances from off the green are considered to be in <strong>yards</strong>, while distances on the green (putts) should be in <strong>feet</strong>, as that's the common golf convention.<br>

As you log your shots, your Strokes Gained will be calculated in the rightmost column, assuming you've entered a valid distance/lie value.<br>Your Strokes Gained by category (<strong>Off the Tee</strong>, <strong>Approach</strong>, <strong>Around the Green</strong>, <strong>Putting</strong>, and <strong>Recovery</strong>) will render as KPIs at the top of the app as you add shots from those situations.

<strong>Screen Recording below</strong>:<br>
https://github.com/user-attachments/assets/4583ee50-6086-4e7c-ba9e-72202a37954a 
<br>

The app defaults to loading in 36 shots - use the "Add Shots" button to add additional shots as needed. Your results can be exported to csv with the "Download" button. Keep in mind that refreshing the page will likely cause you to lose anything you've logged. <br><br>

## Future Roadmap
- Allow for user csv upload
- Additional optional inputs to data table such as club used, etc.
