# bursts
An implementation of Jon Kleinberg's burst detection algorithm.

## Example

```
kleinberg [10,20,30,40,100,101,102,103,104,105,200] $ defOpts
```

## command line example

```
$ bursts-exe 10,20,30,40,100,101,102,103,104,105,200
[0,0,0,0,0,0,3,3,3,3,3,0]
```

## Options

`state = 2`
`gamma = 1`

## install

```
$ stack install
```

## See Also

- Greatly owed implementation and reference test data to [nodejs implementation](https://github.com/hitsujiwool/node-kleinberg-burst).

## License

MIT
