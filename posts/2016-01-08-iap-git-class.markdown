---
title: Teaching Git & Github
author: Greg Hale
tags: git, github, teaching
---

Today was the last of three one-day sessions on [git](https://git-scm.com) and [GitHub](https://github.com) ([here](http://student.mit.edu/searchiap/iap-9289af8f51100344015126db3107020f.html) is the course description).
Next week I'll lead a one-week course called [Opening up your research to the web](http://student.mit.edu/searchiap/iap-9289af8f51100344015126e75301021d.html).

We had 18 students in total, most coming for just a single 1.5 hour session.
Students' ages were anywhere from 17 to 60, with the elder ones tending to be there to listen.
The average reason for attending was more or less:
> Our lab started using some software that's hosted on github, and 
I'm confused about how to keep track of different versions.

My goal in the class was to give students some muscle memory for working with git and GitHub. So I only lectured at the board for 5 minutes or so, then we immediately jumped in to exercises - namely cloning and contributing to a fake repository I set up at [https://github.com/imalsogreg/gitdemo](https://github.com/imalsogreg/gitdemo).

Students forked this repository to their own GitHub accounts (perhaps one still exists [here](https://github.com/yunboyer/gitdemo)?), and each cloned the repository to their laptop, made a change, pushed their work back to github, and then issued pull requests to me.
I integrated each pull request back into the original `gitdemo` repository in turn.
Sometimes the merges were successful. Other times, one student's work being integrated into the central repository would conflict with the merge of a later student's work, so we got to see how merge conflicts are resolved.

It takes a lot more than a couple of hours to get used to version control, learning the jargon, and grasping which aspects of git are mandatory and which are just parts of workflow convention.
But I think the students all came away with a very solid grasp of the basics and how to move forward.
All in all, for git I really like the interactive format.

There are lots of online guides to git, particularly good ones are [try.github.io](https://try.github.io/levels/1/challenges/1) and [git-scm.com](https://git-scm.com/doc).
