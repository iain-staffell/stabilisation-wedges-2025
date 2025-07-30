# written by sasha staffell :-)

# set the total number of wedges to acheive
total_limit = 20

# set the limits for each sector, and for each strategy within the sector
# index 0 is total for the sector
sector_names = ['electricity', 'industry', 'buildings', 'transport', 'land use']

limits = {   # rounded-down average for each sector
	'electricity' : [11, 4, 4, 4, 2, 3, 2, 2, 2],
    'industry' : [14, 3, 2, 2, 2, 1, 1],
    'buildings' : [2, 2, 1, 1],
    'transport' : [8, 2, 4, 1, 1, 1, 1, 1],
    'land use' : [13, 2, 1, 3, 1, 2, 1, 1, 3, 1, 1, 2, 1]
}
# note: set element 0 of the vectors to 15, 18, 3, 10, 18 if you want the upper bound for each sector
#       or to 7, 11, 1, 6, 9 if you want the lower bound of each sector (assuming land is 50% of its upper bound)


# sector_pos stores the possibilities for each total for each sector
# format: {'a' : [1, 12, 71, 12, 1], 'b' : ... and so on
# each number is how many solutions for that total (its index)
sector_pos = {}
for sector in sector_names:
    sector_pos[sector] = []
    for i in range(limits[sector][0]+1):
        sector_pos[sector].append(0) # start with all 0s

# loop through all the sectors, finding solutions for each total
for sector in sector_names:

    # start by using 0 wedges for each strategy
    curr_val = [0 for i in range(len(limits[sector])-1)]
    end = False

    # loop through all possible combinations of wedge deployments
    while True:

        # sum up the current wedges
        curr_sum = sum(curr_val)
        if curr_sum <= limits[sector][0]: # if this is below the sector's limit
            if sector != 'transport':
                sector_pos[sector][curr_sum] += 1 # increase how many solution there are for this total
            else:
                cars_check = sum(curr_val[1:5])
                if cars_check <= 4: # impose an extra check that we can't have >4 passenger transport strategies
                    sector_pos[sector][curr_sum] += 1

        # do all the incrementation of the amounts of this strategy
        # basically just counting, but with different limits for each 'power'
        for i in range(1, len(limits[sector])):
            if curr_val[i-1] < limits[sector][i]: # increase value if we are below the limit
                curr_val[i-1] += 1
                break
            else:
                curr_val[i-1] = 0 # otherwise set it to 0
                if i == len(limits[sector])-1: # if we are at the end, exit
                    end = True
                    break
                
        if end:
            break


# print possible solutions for each total for each sector
for sector in sector_names:
    print(f'There are {sum(sector_pos[sector])} solutions to the {sector} sector:')
    for i in range(len(sector_pos[sector])):
        print(f'\t{sector_pos[sector][i]} solutions that deliver {i} wedges')
    print()

# now find total combinations
total_combos = 0
curr_val = [0 for i in range(len(sector_names))]
end = False

# loop through all solutions for each sector
while True:
    
    # sum up wedges
    curr_sum = sum(curr_val)
    
    # add on this number of combos if it hits our target
    if curr_sum == total_limit:
        # find total combos by multiplying all sectors
        combos = 1
        for i in range(len(sector_names)):
            combos *= sector_pos[sector_names[i]][curr_val[i]]

        # add this to total
        total_combos += combos

    # do the incrementation like last time
    for i in range(len(sector_names)):
        if curr_val[i] < limits[sector_names[i]][0]:
            curr_val[i] += 1
            break
        else:
            curr_val[i] = 0
            if i == 4:
                end = True
                break
            
    if end:
        break

# print total
print('Total amount of combinations:', total_combos)
