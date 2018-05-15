import pandas as pd
import numpy as np

level_to_diff = {'Level 1' : 'Easy', 'Level 2' : 'Medium', 'Level 3' : 'Medium', 'Level 4': 'Hard'}

df = pd.read_csv('log.txt', header=None)
prev_level, prev_player_id, prev_num_acorns = None, None, None
prev_recorded = True

deaths = {}

analytics = {}

for index, row in df.iterrows():
    cur_player_id, cur_level, num_acorns, time, type = row[0], row[1], row[8], row[9], row[10]



    if prev_level == None or cur_level != prev_level or cur_player_id != prev_player_id or 'finish' in type or type == 'death':

        if prev_level != None:

            if 'finish' in type:
                an = analytics[cur_player_id][cur_level]
                an[6].append(row[3])
                prev_recorded = True
                an[2] += 1
                an[7] += time
                an[4].append(row[2])
                an[5] += num_acorns
            elif type == 'death':
                if cur_level not in deaths:
                    deaths[cur_level] = []
                deaths[cur_level].append((row[2],row[3]))

                an = analytics[cur_player_id][cur_level]
                an[6].append(row[3])
                prev_recorded = True
                an[3].append(row[2])
                an[4].append(row[2])
                an[7] += time
                an[5] += num_acorns
            elif not prev_recorded:
                an = analytics[prev_player_id][prev_level]
                an[6].append(row[3])
                an[4].append(row[2])
                an[7] += time
                an[5] += prev_num_acorns

        prev_recorded = False
        if cur_player_id in analytics:
            player_analytics = analytics[cur_player_id]
            if cur_level in player_analytics:
                an = player_analytics[cur_level]
                an[1] += 1 # Incremenet Attempt Count

            else:
                player_analytics[cur_level] = [level_to_diff[cur_level], 1, 0, [], [], num_acorns, [row[3]], 0]
        else:
            analytics[cur_player_id] = {cur_level : [level_to_diff[cur_level], 1, 0, [], [], num_acorns, [row[3]], 0]}

    else:
        an = analytics[cur_player_id][cur_level]
        an[6].append(row[3])



    prev_player_id = cur_player_id
    prev_level = cur_level
    prev_num_acorns = num_acorns


def m(i, val):
    if i == 3:
        return np.var(val) if len(val) > 0 else 0
    elif i == 4:
        return max(val) if len(val) > 0 else 0
    elif i == 6:
        return np.mean(val) if len(val) > 0 else 0
    else:
        return val



results = []


for p_id, p_an in analytics.items():
    for level, an in p_an.items():
        data = ['Sky Knight', p_id, level] + [m(i, val) for i, val in enumerate(an)]
        results.append(data)

df = pd.DataFrame(results, columns=['Game ID', 'Player ID', 'Level ID', 'Objective Difficulty', 'Number of Attempts','Number of Successful Attempts', 'Variance of x-Coordinate of Death Locations', 'Maximum Reached x-Coordinate', 'Acorns Collected', 'Average y-Coordinate of Player', 'Total  Time Spent' ])
df.to_csv('analytics/data.csv')

import matplotlib.pyplot as plt
import numpy as np
from matplotlib.patches import Circle

for level, coor in deaths.items():

    img = plt.imread('levels/' + level.lower().replace(' ','') + '/art.png')

    # Create a figure. Equal aspect so circles look circular
    fig,ax = plt.subplots(1)
    ax.set_aspect('equal')

    # Show the image
    ax.imshow(img)

    # Now, loop through coord arrays, and create a circle at each x,y pair
    for x,y in deaths[level]:
        circ = Circle((x/20 + 1500,y/-20 + 750),7, color='black')
        ax.add_patch(circ)

    plt.savefig('analytics/' + level.lower().replace(' ','') + '_heatmap.png')
