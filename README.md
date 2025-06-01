# stRokes_gained

## Calculate Your Own Strokes Gained
Welcome to the stRokes_gained repo - an intersection of golf and data!<br>

 Try the Shiny app to see how your game stacks up: https://abodesy14.shinyapps.io/stRokes_gained/

## Why Use Strokes Gained?<br>
"Strokes Gained" is a modern approach to evaluating golfer performance. Rather than tracking traditional stats like fairways hit, greens in regulation, or total putts, strokes gained puts each shot in context by comparing it to the average outcome for golfers of specific handicap levels.

These averages are derived from millions of shots across different lies/distance/handicap combinations, allowing us to estimate the expected number of strokes from any position on the course. This makes it easier to identify where you're gaining or losing strokes, and where to focus your practice.

## Real-World Example:<br>

<img width="310" alt="15 yards in sand" src="https://github.com/user-attachments/assets/56cc6a11-ff96-49ee-ba0f-020c2c2c7f50" width=45%>

Take for example the 400-yard hole in the image above. The PGA Tour average for a hole of this length is <strong>3.98 strokes</strong>. Imagine you're a PGA Tour pro for a second, playing this hole:

<strong>Shot 1</strong>: You tee off from 400 yards, ending up at 100 yards in the fairway. A great shot, any way you slice it. Now, the expected strokes for a PGA Tour pro from 100 yards in the fairway is <strong>2.78</strong> strokes. It obviously took one stroke to get there, so we can calculate the strokes gained on the tee shot as:<br>
(3.98 - 1) - 2.78 = <strong>+0.2 strokes gained</strong><br>
<strong>Shot 2</strong>: Imagine from there (100 yards from the pin in the fairway), your 2nd shot ends up in the sand, 15 yards from the pin. The expected strokes from there is <strong>2.5.</strong><br>
(2.78 - 1) - 2.5 = <strong>-0.72 strokes "lost"</strong><br>
<strong>Shot 3</strong>: Now, you hit your bunker shot to 6 feet on the green, which carries an expected stroke value of <strong>1.34.</strong><br>
(2.5 - 1) - 1.34 = <strong>+0.16 strokes gained</strong><br>
<strong>Shot 4</strong>: You hole your putt for par:<br>
1.34 - 1 = <strong>+0.34 strokes gained</strong><br><br>

In aggregate, you <strong>lost -0.02 strokes</strong> on the hole (4 - 3.98). You made a par on this hole, but each shot had its own value — some positive, some negative. Over time, this type of analysis can surface trends and uncover the true strengths and weaknesses in your game.
