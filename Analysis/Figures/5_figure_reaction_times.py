#!/usr/bin/env python3

import os.path as op
import numpy as np
import pandas as pd
import scipy.stats as ss
import matplotlib.pyplot as plt
from matplotlib import font_manager
from matplotlib.gridspec import GridSpec
from matplotlib.colors import LinearSegmentedColormap, Normalize
from matplotlib.lines import Line2D
from matplotlib.collections import LineCollection
from matplotlib.offsetbox import AnchoredText

from seaborn import kdeplot

savefig = True


def peak_of_chisq(data):
    import scipy.stats as ss
    df, loc, scale = ss.chi2.fit(data, floc=0)
    peak = np.maximum(0, (df - 2)) * scale
    return peak


# load data
data = pd.read_csv(op.join('..', 'cleaned_data.csv'))
data = data[['subject_id', 'group', 'continuum', 'paradigm', 'wj_brs', 'step',
             'RT']]
groups = ['Dyslexic', 'Below Average', 'Above Average']
continua = ['/ʃa/-/sa/', '/ba/-/da/']
linedict = {k: v for k, v in zip(continua, ['dashed', 'solid'])}

# censor reaction times to reasonable values
censored_data = data.loc[(data['RT'] > 0.25) & (data['RT'] < 5)]

# colors
colors = ['#3a5fcd', '#cd2626']  # royalblue3, firebrick3
cmap = LinearSegmentedColormap.from_list(name='wjbrs', colors=colors, N=256)
subjs = data.groupby('subject_id').agg(dict(wj_brs=np.unique, group=np.unique))
wjbrs_min, wjbrs_max = subjs['wj_brs'].min(), subjs['wj_brs'].max()
wjbrs_range = wjbrs_max - wjbrs_min
subjs['wj_brs'] = (subjs['wj_brs'] - subjs['wj_brs'].min()) / wjbrs_range
group_means = subjs.groupby('group').aggregate('mean')
colordict = {k: cmap(v) for k, v in group_means.itertuples()}

'''
# sanity check
fig, ax = plt.subplots()
for ix, this_group in enumerate(groups):
    this_data = data.loc[data['group'] == this_group]
    ax.scatter(this_data['step'], this_data['RT'], alpha=0.3, color=colors[ix])
    coefs = np.polyfit(this_data['step'], this_data['RT'], deg=2)
    p = np.poly1d(coefs)
    x = np.linspace(1, 7, 61)
    ax.plot(x, p(x), color=colors[ix])
plt.ion()
plt.show()
'''

# aggregate
gb = censored_data.groupby(['subject_id', 'continuum', 'paradigm', 'step'])
agg_data = gb.aggregate(dict(group=np.unique, wj_brs=np.unique,
                             RT=peak_of_chisq))
agg_data.reset_index(inplace=True)

# init figure
plt.style.use({'font.sans-serif': 'Noto Sans'}) #,#'font.family': 'sans-serif'})
fig = plt.figure(figsize=(6, 9))
gs = GridSpec(9, 7, left=0.12, right=0.94, bottom=0.05, top=0.95,
              wspace=0.3, hspace=0.1,
              height_ratios=[1, 1, 0.5, 1.5, 1.5, 1.5, 1.5, 1.2, 3],
              width_ratios=[3, 3, 3, 3, 3, 3, 1])

# add empty plots as buffer space between rows
for row in [1, -2]:
    ax = fig.add_subplot(gs[row, :])
    ax.axis('off')

# histogram / chisquared fit / kernel density plot
ax = fig.add_subplot(gs[0, :])
x = np.linspace(0, 20, 401)    # 50 ms bins
xx = np.linspace(0, 20, 4001)  # denser sampling for smoother fitted curve
for group in groups[::-1]:
    color = colordict[group]
    edgecolor = tuple(list(color[:-1]) + [0.6])
    facecolor = tuple(list(color[:-1]) + [0.2])
    this_data = agg_data.loc[agg_data['group'] == group]
    df, loc, scale = ss.chi2.fit(this_data['RT'], floc=0)
    # ax.hist(this_data['RT'], bins=x, normed=True, histtype='stepfilled',
    #         facecolor=facecolor, edgecolor=edgecolor, label=group,
    #         linewidth=0.5)
    # ax.plot(xx, ss.chi2.pdf(xx, df, scale=scale), color=color, linewidth=1.5)
    kdeplot(this_data['RT'], shade=True, color=color, ax=ax, label=group,
            linewidth=1.5)
# title = 'Reaction time by group ({} ms bins)'.format(int(x[1] * 1000))
title = 'Kernel density plots of reaction time distribution by group'
ax.set_xlim(0, 2)
ax.set_xticks(np.linspace(0, 2, 5))
ax.set_title(title)
ax.set_xlabel('Reaction time (s)')
ax.set_ylabel('Normalized\ndensity')
ax.legend(loc='right', frameon=False, labelspacing=0.3)

# fake titles for next row
for ix, group in enumerate(groups):
    ax = fig.add_subplot(gs[2, (2*ix):(2*ix+2)])
    ax.set_facecolor(colordict[group])
    prop = dict(color='w', backgroundcolor=colordict[group], fontweight='bold')
    title = AnchoredText(group, loc=10, pad=0, prop=prop)
    ax.add_artist(title)
    ax.xaxis.set_visible(False)
    ax.yaxis.set_visible(False)

# fitted models per subject
coefs_df = pd.DataFrame(np.nan, index=agg_data.groupby('subject_id').groups,
                        columns=continua)
x = np.linspace(1, 7, 61)
norm = Normalize(vmin=wjbrs_min, vmax=wjbrs_max)
for ix, group in enumerate(groups):
    for iy, continuum in enumerate(continua):
        # setup subplot
        xcontext = {'ytick.major.size': 0} if ix else dict()
        ycontext = dict() if iy else {'xtick.major.size': 0}
        with plt.style.context([xcontext, ycontext]):
            ax = fig.add_subplot(gs[(2*iy+3):(2*iy+5), (2*ix):(2*ix+2)])
        # process data
        this_group = agg_data.loc[(agg_data['group'] == group) &
                                  (agg_data['continuum'] == continuum)]
        by_subj = this_group.groupby('subject_id')
        fits = list()
        for subj in by_subj.groups:
            this_subj = by_subj.get_group(subj)
            coefs = np.polyfit(this_subj['step'], this_subj['RT'], deg=2)
            coefs_df.loc[subj, continuum] = coefs[0]
            p = np.poly1d(coefs)
            fits.append(p(x))
        segs = LineCollection([list(zip(x, y)) for y in fits], linewidths=1.5,
                              linestyles=linedict[continuum], norm=norm)
        segs.set_array(by_subj.agg(dict(wj_brs=np.unique))['wj_brs'].values)
        segs.set_cmap(cmap)
        ax.add_collection(segs)
        # garnishes
        ax.set_xlim(0.75, 7.25)
        ax.set_ylim(0., 1.1)
        ax.set_xticks(range(1, 8))
        ax.set_yticks(np.linspace(0, 1, 6))
        ax.grid(color='0.9', linewidth=1)
        if not iy:
            ax.set_xticklabels(['' for _ in ax.get_xticks()])
        if ix:
            ax.set_yticklabels(['' for _ in ax.get_yticks()])
            if (ix == 1) and iy:
                ax.set_xlabel('Continuum step')
        else:
            c = continuum
            ax.set_ylabel('{}\nReaction time (s)'.format(c))
# colorbar
with plt.style.context({'ytick.direction': 'in', 'ytick.color': 'w'}):
    cax = fig.add_subplot(gs[4:6, -1])
    cbar = fig.colorbar(segs, cax=cax)
    ticks = np.array([60, 80, 100, 120])
    cbar.set_ticks(ticks)
    cbar.outline.set_linewidth(0)
    cbar.set_clim(wjbrs_min, wjbrs_max)
    _ = [l.set_color('k') for l in cax.get_yticklabels()]
    cax.set_title('WJ-BRS', loc='left')

# mean fitted model for all groups
ax = fig.add_subplot(gs[-1, :2])
by_group = agg_data.groupby(['group', 'continuum'])
for group, continuum in by_group.groups:
    this_group = by_group.get_group((group, continuum))
    coefs = np.polyfit(this_group['step'], this_group['RT'], deg=2)
    p = np.poly1d(coefs)
    x = np.linspace(1, 7, 61)
    ax.plot(x, p(x), linewidth=1.5, linestyle=linedict[continuum],
            color=colordict[group])
ax.set_xticks(range(1, 8))
ax.set_ylim(0.3, 0.65)
ax.set_yticks(np.linspace(0.3, 0.6, 4))
ax.set_ylabel('Reaction time (s)')
# ax.grid(color='0.9', linewidth=1)
linetypes = [Line2D([], [], color='k', linestyle=s, label=l)
             for l, s in linedict.items()]
ax.legend(handles=linetypes, loc='lower center', frameon=False)  # prop=fp
ax.set_title('Group-level fits')

# barplot of quadratic coefs
group_table = agg_data[['subject_id', 'group']].copy().drop_duplicates()
coefs_df = coefs_df.reset_index().rename(columns=dict(index='subject_id')
                                         ).merge(group_table)
means = coefs_df.copy().drop(['subject_id'], axis=1
                             ).groupby(['group']).agg(np.mean)
sterrs = coefs_df.copy().drop(['subject_id'], axis=1
                              ).groupby('group').agg((lambda x: np.std(x) /
                                                      np.sqrt(x.size)))
plt.style.use({'xtick.major.size': 0})
ax = fig.add_subplot(gs[-1, 3:])
for ix, continuum in enumerate(means.columns):
    x_pos = [[2, 1, 0], [6, 5, 4]][ix]
    cols = pd.Series(means.index).map(colordict)
    ax.bar(x_pos, means[continuum], yerr=sterrs[continuum],
           color=pd.Series(means.index).map(colordict), capsize=3)
ax.set_ylim(0, -0.011)
ax.set_yticks(np.linspace(0, -0.01, 3))
ax.set_xticks([1, 5])
ax.set_xticklabels(means.columns.str.replace('-', '~'))
ax.set_title('Quadratic coefficients')
# legend
# ax.legend(loc='upper left', bbox_to_anchor=(1.1, 1), frameon=False,
#           labelspacing=1.2, handlelength=1.5)

# save
if savefig:
    fig.savefig(op.join('..', 'figure_reaction_times.pdf'))
else:
    plt.ion()
    plt.show()
