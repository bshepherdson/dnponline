#!/bin/sh

gzip -c dnp.warp | ssh diceandp@diceandpaper.com "gzip -d > dnponline/dnp.warp"
scp static/table.js diceandp@diceandpaper.com:dnponline/static

