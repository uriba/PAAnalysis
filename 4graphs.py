import csv
from matplotlib.pyplot import plot,show,figure,subplot,ylim

def ploti(i):
    nums = []
    with open('graph%d.csv' % (i),'rb') as csvfile:
        vals = csv.reader(csvfile,delimiter=',',quotechar='"')
        for row in vals:
            nums.append(map(float,row))
    return nums

fig = figure()
anums = []
for i in range(1,5):
    anums.append(ploti(i))
a1 = subplot(221)
a1.plot(anums[0][0],anums[0][1],anums[0][2],anums[0][3],anums[0][4],anums[0][5])

a2 = subplot(222,sharey=a1, sharex = a1)
a2.plot(anums[1][0],anums[1][1],anums[1][2],anums[1][3],anums[1][4],anums[1][5])

a3 = subplot(223,sharey=a1, sharex = a1)
a3.plot(anums[2][0],anums[2][1],anums[2][2],anums[2][3],anums[2][4],anums[2][5])
a4 = subplot(224,sharey=a1, sharex = a1)
a4.plot(anums[3][0],anums[3][1],anums[3][2],anums[3][3],anums[3][4],anums[3][5])

show()
