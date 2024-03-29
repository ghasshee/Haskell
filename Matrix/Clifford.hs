module Matrix.Clifford where

import Matrix.Matrix


e = Mat [
    [ 1, 0, 0, 0, 0, 0, 0, 0],
    [ 0, 1, 0, 0, 0, 0, 0, 0],
    [ 0, 0, 1, 0, 0, 0, 0, 0],
    [ 0, 0, 0, 1, 0, 0, 0, 0],
    [ 0, 0, 0, 0, 1, 0, 0, 0],
    [ 0, 0, 0, 0, 0, 1, 0, 0],
    [ 0, 0, 0, 0, 0, 0, 1, 0],
    [ 0, 0, 0, 0, 0, 0, 0, 1]]

a = Mat [
    [ 0,-1, 0, 0, 0, 0, 0, 0],
    [ 1, 0, 0, 0, 0, 0, 0, 0],
    [ 0, 0, 0, 1, 0, 0, 0, 0],
    [ 0, 0,-1, 0, 0, 0, 0, 0],
    [ 0, 0, 0, 0, 0, 1, 0, 0],
    [ 0, 0, 0, 0,-1, 0, 0, 0],
    [ 0, 0, 0, 0, 0, 0, 0,-1],
    [ 0, 0, 0, 0, 0, 0, 1, 0]]

b = Mat [
    [ 0, 0,-1, 0, 0, 0, 0, 0],
    [ 0, 0, 0,-1, 0, 0, 0, 0],
    [ 1, 0, 0, 0, 0, 0, 0, 0],
    [ 0, 1, 0, 0, 0, 0, 0, 0],
    [ 0, 0, 0, 0, 0, 0, 1, 0],
    [ 0, 0, 0, 0, 0, 0, 0, 1],
    [ 0, 0, 0, 0,-1, 0, 0, 0],
    [ 0, 0, 0, 0, 0,-1, 0, 0]]

c = Mat [
    [ 0, 0, 0,-1, 0, 0, 0, 0],
    [ 0, 0, 1, 0, 0, 0, 0, 0],
    [ 0,-1, 0, 0, 0, 0, 0, 0],
    [ 1, 0, 0, 0, 0, 0, 0, 0],
    [ 0, 0, 0, 0, 0, 0, 0, 1],
    [ 0, 0, 0, 0, 0, 0,-1, 0],
    [ 0, 0, 0, 0, 0, 1, 0, 0],
    [ 0, 0, 0, 0,-1, 0, 0, 0]]

ii = a * b
jj = b * c 
kk = c * a




i = Mat [
    [ 0,-1, 0, 0],
    [ 1, 0, 0, 0],
    [ 0, 0, 0, 1],
    [ 0, 0,-1, 0]]

j = Mat [
    [ 0, 0,-1, 0],
    [ 0, 0, 0,-1],
    [ 1, 0, 0, 0],
    [ 0, 1, 0, 0]]

k = Mat [
    [ 0, 0, 0,-1],
    [ 0, 0, 1, 0],
    [ 0,-1, 0, 0],
    [ 1, 0, 0, 0]]

r = Mat [
    [ 1, 0, 0, 0],
    [ 0,-1, 0, 0],
    [ 0, 0,-1, 0],
    [ 0, 0, 0, 1]]

s = Mat [
    [ 0, 1, 0, 0],
    [ 1, 0, 0, 0],
    [ 0, 0, 0, 1],
    [ 0, 0, 1, 0]]

t = Mat [
    [ 0, 0, 1, 0],
    [ 0, 0, 0,-1],
    [ 1, 0, 0, 0],
    [ 0,-1, 0, 0]]

u = Mat [
    [ 0, 0,-1, 0],
    [ 0, 0, 0, 1],
    [ 1, 0, 0, 0],
    [ 0,-1, 0, 0]]

ui = u * i
uj = u * j
uk = u * k


