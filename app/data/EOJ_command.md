## EOJ command

**Summary**

**Privileges**
System manager or User 0

**Function**
Specifies end of job and initiates Model 204 termination, forcing active users off Model 204

**Syntax**
EOJ

**Usage notes**
* The EOJ command signals an end-of-job condition and begins the process of Model 204 termination. It should be used in conjunction with the EOD and BUMP commands. Any users who are still active when EOJ is issued are forced off Model 204. If files are being updated at this time, their integrity might be damaged.
* You can include the EOJ command in the User 0 input stream (CCAIN). When the EOJ command is invoked from CCAIN, the Online is immediately terminated, without any of the prompts described on the rest of this wiki page.
* If the end of the User 0 input stream is reached without an EOJ command, a user restart is invoked and the return code is set to 6.
* If you enter an EOJ command from a terminal, you must confirm the command before it is executed:
    * If you are on a full screen thread and the SIRPRMPT X'04' bit is on, you are prompted for the name of the Online for confirmation.
    * If you are on a line-at-a-time thread and SIRPRMPT X'08' is on, you are prompted for the name of the Online for confirmation.
    * If SIRPRMPT has not been set, you are prompted whether to end the run (Yes or No).
    * If the your response is Yes, Model 204 is terminated. If your response is No, the EOJ command is ignored. The default response is Y if the PROMPT parameter includes the X'10' bit.
    * The name of the Online is the Online's job name if CUSTOM is set to 37, or the location of the Online if the LOCATION parameter is set.

**Examples**
* If CUSTOM=37 is set:
    * M204.1076: Do you really want to end the run for jobname?
* If CUSTOM=37 is not set, and LOCATION is set:
    * M204.1076: Do you really want to end the run for location?
* If CUSTOM=37 is not set, and LOCATION is not set:
    * M204.1076: Do you really want to end the run?
* If the RESTART command (ROLL BACK or ROLL FORWARD) is executing when the EOJ command is entered, the following message is displayed prior to the request for confirmation:
    * M204.1453: RESTART RECOVERY IS RUNNING
* No confirmation is requested when the EOJ command is supplied by User 0, either in the CCAIN input stream or from the console in a single-user z/VM run.
* If the EOJPW parameter was set, the following prompt will appear, and you must enter the value of EOJPW to end the run:
    * M204.0347: Password

**Checkpoint logging**
If checkpoint logging is enabled in the Model 204 run, the EOJ command requests that a checkpoint be performed before Model 204 termination processing begins. This checkpoint request waits until the values of the CPTO and CPTQ parameters are exceeded. If updating users are finished, their work is captured by the checkpoint, otherwise it might be lost. After the checkpoint is taken or times out, termination processing proceeds.
When Model 204 processes EOJ, it abruptly ends all update units in progress and does not invoke transaction backout.

For more information about:
* Model 204 update units, see Update units and transactions
* EOJ and ONLINE termination, see ONLINE termination.

**Categories:** System manager commands | Commands
