# `gfs`

## What problem does it solve?

If you make backups of your data regularly (and you should), you'll get a lot of e.g. daily backups a couple of months ago even though you don't really need that granularity the further back in time you go. A backup cleanup system like [Grandfather-Father-Son](https://en.wikipedia.org/wiki/Backup_rotation_scheme#Grandfather-father-son) can be used to remove older backups while still leaving one in every time period, for example, one every month or one every year after the first year.

`gfs` is a small command-line program to implement such an algorithm; it works as a filter reading backup names with dates from `stdin` and printing which of them need to be removed to `stdout`. It does not know how to list backups from your system and how to remove them; your commands should do that, giving you more freedom to do exactly what you need.

For now, the cleanup ranges for the algorithm are fixed, that is it will leave:
* one backup every hour for a day (24 hours);
* one backup every day for a month;
* one backup every week for a year;
* one backup every month for 4 years;
* one backup every year for 32 years.

And there are a few extra rules:
* The newest backup (before "now") is always kept.
* All backups that are newer than "now" are always kept (even though it shouldn't happen in real life).

An example with times, assuming "now" (the system time when running the command) is "2023-04-02 10:50:00":

```bash
$ echo '2021-12-04-000000
2021-12-10-000000
2021-12-31-083000
2023-01-10-150010
2023-01-10-200000
2023-01-10-220000
2023-02-06-005500
2023-02-06-005800
2023-02-06-005900' | gfs -f '%Y-%m-%d-%H%M%S'
2021-12-10-000000
2021-12-31-083000
2023-01-10-200000
2023-01-10-220000
2023-02-06-005800
```

Here it prints three times to be removed. The reason is explained with the input times:

```
# monthly because the dates are more than a year away from now:
2021-12-04-000000  # only one time during the month of [2021-11-06 08:00:00; 2021-12-06 08:00:00],
                  # so it is kept
## these two are within the same month of [2021-12-06 08:00:00; 2022-01-06 08:00:00]:
2021-12-10-000000  # thus the older one is kept
2021-12-31-083000  # <<< while the other one is removed

# daily because the dates are within a month from now:
## these are within the same day of [2023-01-10 08:00:00; 2023-01-11 08:00:00]:
2023-01-10-150010  # thus the older one is kept
2023-01-10-200000  # <<< while all the other ones…
2023-01-10-220000  # <<< …are removed

# hourly because the dates are within 24 hours from now:
## these are within the same hour of [2023-02-06 00:00:00; 2023-02-06 01:00:00]:
2023-02-06-005500  # thus the older one is kept
2023-02-06-005800  # <<< while all the other ones are removed
2023-02-06-005900  # except for the newest time before "now" 2023-02-06 08:00:00
```

Note that the program uses calendar-based calculations for month differences clipping the result to valid days. This behavior is visible at the end of some months: `2023-05-31 - 1 month = 2023-04-30`. Due to this, more days are included in this 1 month's interval than usual, and you can see more times to be removed if you run the program at the end of a month.

The program is inspired by [`tarsnapper`](https://github.com/miracle2k/tarsnapper). The difference is `tarsnapper` works only with `tarsnap` and it has other functionality in addition to expiring backups.

There is more background information about the project in my article at <https://egeek.me/2023/02/12/the-gfs-program/>.

### Lenient match

The `--lenient-match` option asks `gfs` to search for time (matching the provided pattern) inside the input strings, which may have arbitrary prefixes and/or suffixes. Here's an example from above with prefixes and suffixes, producing the same dates to cleanup:

```bash
$ echo 'host0 2021-12-04-000000
host1 2021-12-10-000000
foo 2021-12-31-083000
2023-01-10-150010
host0 2023-01-10-200000
2023-01-10-220000
before 2023-02-06-005500 after
2023-02-06-005800
foo 2023-02-06-005900' | gfs -f '%Y-%m-%d-%H%M%S' --lenient-match
host1 2021-12-10-000000
foo 2021-12-31-083000
host0 2023-01-10-200000
2023-01-10-220000
2023-02-06-005800
```

This is useful e.g. when your backup names include variable data (an identifier) and you need that data to remove backups.

Note: the prefix shouldn't end with a digit and the suffix shouldn't start with a digit because that may confuse the time parser. For example, `foo12023-02-01-150000` might be parsed as year `12023`.

## Examples

### `tarsnap`

My primary use case for the program is to clean up older [Tarsnap](https://www.tarsnap.com/) backups because the official tool doesn't support any cleanup strategies. It's typical to include time in the backup names since those must be unique.

First you need to figure out the time format string for your backup names using the specifiers available in the `time` library: <https://hackage.haskell.org/package/time-1.12.2/docs/Data-Time-Format.html#v:formatTime>. If your backup names look like `home-2023-01-31_23-59-59`, then the string would be `home-%Y-%m-%d_%H-%M-%S`. The string has to match the entire name.

Then you need to list the backups and pipe them through `gfs`, which could look like this:

```bash
$ tarsnap --keyfile remove.ts --list-archives | tee tarsnap.lst | gfs -f 'home-%Y-%m-%d_%H-%M-%S' | sed 's/^/-f /' | xargs -p tarsnap --keyfile remove.ts -d -v --print-stats --humanize-numbers
```

or multiline with comments:

```bash
# list the backups
$ tarsnap --keyfile remove.ts --list-archives \
# save the list to a file in addition to piping it to gfs
    | tee tarsnap.lst \
# run gfs to figure out which backups to remove using the format string from above
    | gfs -f 'home-%Y-%m-%d_%H-%M-%S' \
# prepend `-f ` to every backup in order to call tarsnap with these arguments
    | sed 's/^/-f /' \
# delete the backups after asking for a confirmation
    | xargs -p tarsnap --keyfile remove.ts -d -v --print-stats --humanize-numbers
```

A small unrelated note is that the `tarsnap` commands use the `--keyfile remove.ts` argument, which tells it to use a separate key file that has the permission to list and remove backups. It's a good security practice to have the default key with the write-only permission. Read more at <https://www.tarsnap.com/security.html>.

Example output:

```bash
tarsnap --keyfile remove.ts -d -v --print-stats --humanize-numbers -f home-2023-01-05_00-09-59 -f home-2023-01-08_17-24-27 -f home-2023-01-31_05-31-37?...y
Deleting archive "home-2023-01-05_00-09-59"
                                       Total size  Compressed size
All archives                               6.1 GB           5.0 GB
  (unique data)                            277 MB           166 MB
home-2023-01-05_00-09-59                    93 MB            75 MB
Deleted data                               351 kB           106 kB
Deleting archive "home-2023-01-08_17-24-27"
                                       Total size  Compressed size
All archives                               6.0 GB           4.9 GB
  (unique data)                            275 MB           165 MB
home-2023-01-08_17-24-27                    93 MB            75 MB
Deleted data                               2.2 MB           812 kB
Deleting archive "home-2023-01-31_05-31-37"
                                       Total size  Compressed size
All archives                               5.9 GB           4.8 GB
  (unique data)                            275 MB           165 MB
home-2023-01-31_05-31-37                   107 MB            89 MB
Deleted data                               5.1 kB           5.7 kB
```

### `restic`

`gfs` can be used with [`restic`](https://restic.net/). Note that it already has a similar [snapshots removal policy](https://restic.readthedocs.io/en/stable/060_forget.html#removing-snapshots-according-to-a-policy), however its options are different and `gfs` might be more appropriate for you.

`restic` (unlike `tarsnap`) identifies backups (snapshots) with a hash and their dates are secondary. Looks like the backup dates in the json format use a fixed format, which can be parsed with `%Y-%m-%dT%H:%M:%S.%-q%Ez`. The basic command can look like:

```bash
$ restic -r home snapshots --json | tee restic.lst | jq -r 'map(.time + "|" + .short_id) | .[]' | gfs -f '%Y-%m-%dT%H:%M:%S.%-q%Ez' --lenient-match | cut -d'|' -f2 | xargs -p restic -r home -v forget --prune
```

or multiline:

```bash
# list the backups
$ restic -r home snapshots --json \
# save the list to a file in addition to piping it to gfs
    | tee restic.lst
# print backup's time and identifier, one backup per line
    | jq -r 'map(.time + "|" + .short_id) | .[]' \
# run gfs to figure out which backups to remove using the default format string
    | gfs -f '%Y-%m-%dT%H:%M:%S.%-q%Ez' --lenient-match \
# extract the identifier of each backup to cleanup
    | cut -d'|' -f2 \
# delete the backups after asking for a confirmation
    | xargs -p restic -r home -v forget --prune
```

For reference, the `jq` command produces a list like this:

```
2021-09-19T10:31:06.382683+03:00|922246b0
2021-10-14T16:34:13.432662+03:00|7895239a
2021-11-11T05:58:33.99914+02:00|722b7def
```

A more real-life example where `restic` is run as a superuser (to make backups of the entire system), so we need `sudo` with environment variables:

```bash
# list the backups, passing the necessary environment variables to the root session
$ sudo --preserve-env=AWS_SECRET_ACCESS_KEY,AWS_ACCESS_KEY_ID,RESTIC_PASSWORD_COMMAND,RESTIC_REPOSITORY \
    restic -r "${RESTIC_REPOSITORY}/home" snapshots --json \
# save the list to a file in addition to piping it to gfs
    | jq -r 'map(.time + "|" + .short_id) | .[]' \
# run gfs to figure out which backups to remove using the default format string
    | gfs -f '%Y-%m-%dT%H:%M:%S.%-q%Ez' --lenient-match \
# extract the identifier of each backup to cleanup
    | cut -d'|' -f2 \
# delete the backups after asking for a confirmation, `caffeinate` prevents macos
# from sleeping while the command is running; `noti` displays a notification when
# the command finishes; `time` shows how long the command took
    | xargs -p caffeinate noti time sudo --preserve-env=AWS_SECRET_ACCESS_KEY,AWS_ACCESS_KEY_ID,RESTIC_PASSWORD_COMMAND,RESTIC_REPOSITORY \
    restic -r "${RESTIC_REPOSITORY}/home" -v forget --prune
```

Example output:

```bash
Password:
caffeinate noti time sudo --preserve-env=AWS_SECRET_ACCESS_KEY,AWS_ACCESS_KEY_ID,RESTIC_PASSWORD_COMMAND,RESTIC_REPOSITORY restic -r s3:s3.amazonaws.com/bucket_name/home -v forget --prune 922246b0?...y
repository 90b841ae opened (version 1)
[0:00] 100.00%  1 / 1 files deleted
1 snapshots have been removed, running prune
loading indexes...
loading all snapshots...
finding data that is still in use for 99 snapshots
[4:20] 100.00%  99 / 99 snapshots
searching used packs...
collecting packs for deletion and repacking
will remove pack 4ba6104b as it is unused and not indexed
[0:29] 100.00%  999078 / 999078 packs processed
…
removing 80 old packs
[0:09] 100.00%  80 / 80 files deleted
done
      515.76 real      1270.59 user       212.15 sys
```

## TODO

* Ignore unparseable strings instead of stopping with an error.
* User-configurable GFS ranges.
* Use a custom "now" time if provided (for debugging).

## Building

To build the program, you need to install the [Haskell Stack](https://docs.haskellstack.org/en/stable/) and run `stack build` in the project directory.

To execute the tests, run `stack test`.
