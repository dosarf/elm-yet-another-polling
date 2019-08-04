# elm-yet-another-polling
Yet Another Polling package with
* min and max retry time intervals
* and backing-off strategy.

You define
* a function creating tasks to attempt (mostly HTTP task, but it could
  also be something else),
* another function that interprets the results of such task and decides
  how to continue with polling,
* and finally a function to turn the results of your task into a message
  of yours.

You must feed back the result to the `update` method, and that's it.
