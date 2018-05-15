import pandas as pd
import numpy as np

level_to_diff = {'Level 1' : 'Easy', 'Level 2' : 'Medium', 'Level 3' : 'Medium', 'Level 4': 'Hard'}

df = pd.read_csv('log.txt', header=None)
prev_level, prev_player_id, prev_num_acorns = None, None, None
prev_recorded = True

analytics = {}




for index, row in df.iterrows():
    cur_player_id, cur_level, num_acorns, type = row[0], row[1], row[8], row[9]



    if prev_level == None or cur_level != prev_level or cur_player_id != prev_player_id or 'finish' in type or type == 'death':

        if prev_level != None:

            if 'finish' in type:
                an = analytics[cur_player_id][cur_level]
                an[6].append(row[3])
                prev_recorded = True
                an[2] += 1
                an[4].append(row[2])
                an[5] += num_acorns
            elif type == 'death':
                an = analytics[cur_player_id][cur_level]
                an[6].append(row[3])
                prev_recorded = True
                an[3].append(row[2])
                an[4].append(row[2])
                an[5] += num_acorns
            elif not prev_recorded:
                an = analytics[prev_player_id][prev_level]
                an[6].append(row[3])
                an[4].append(row[2])
                an[5] += prev_num_acorns

        prev_recorded = False
        if cur_player_id in analytics:
            player_analytics = analytics[cur_player_id]
            if cur_level in player_analytics:
                an = player_analytics[cur_level]
                an[1] += 1 # Incremenet Attempt Count

            else:
                player_analytics[cur_level] = [level_to_diff[cur_level], 1, 0, [], [], num_acorns, [row[3]]]
        else:
            analytics[cur_player_id] = {cur_level : [level_to_diff[cur_level], 1, 0, [], [], num_acorns, [row[3]]]}

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

df = pd.DataFrame(results, columns=['Game ID', 'Player ID', 'Level ID', 'Objective Difficulty', 'Number of Attempts','Number of Successful Attempts', 'Variance of x-Coordinate of Death Locations', 'Maximum Reached x-Coordinate', 'Acorns Collected', 'Average y-Coordinate of Player' ])
df.to_csv('analytics.csv')
