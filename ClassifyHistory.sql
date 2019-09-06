#SQL for cleaning data

#Add extra columns in the message
ALTER TABLE Messages
ADD COLUMN hist_UploadType VARCHAR(30),
ADD COLUMN hist_VoteScore TINYINT(4),
ADD COLUMN hist_IsAuto TINYINT(4),
ADD COLUMN hist_FeedbackType VARCHAR(20),
ADD COLUMN hist_ignore TINYINT(4),
ADD COLUMN hist_isPatchOwner TINYINT(4);


#Identify automatically generated message
update Messages as hist join Accounts as p on hist.author = p._account_id
set hist_isAuto = 1
where p.IsBotAccount = 1; 

#Create a new table that do not include bot messages
CREATE TABLE Activity LIKE Messages; 
INSERT Activity SELECT * FROM Messages;
DELETE FROM Activity WHERE hist_isAuto = 1;

#Identify the patch upload 
update Activity
set hist_UploadType = 'New'
where message like 'Uploaded%' and _revision_number = 1;


#Identify the pactch revised 
update Activity
set hist_UploadType = 'Revised'
where (message like 'Uploaded%' and _revision_number > 1) or message REGEXP 'Patch Set [0-9]+: Published edit on patch set' ;


#Identify the pactch rebased 
update Activity
set hist_UploadType = 'Rebased'
where message like '%was rebased%' or message REGEXP 'Patch Set [0-9]+: Rebased';

#Identify the pactch cherry pick 
update Activity
set hist_UploadType = 'Cherry-Picked from'
where message REGEXP 'Patch Set [0-9]+: Cherry Picked from';

update Activity
set hist_UploadType = 'Cherry-Picked to'
where message Like '%This patchset was cherry picked to%' or message Like  'Change has been successfully cherry-picked as %';

#Identify the pactch cherry pick 
update Activity
set hist_UploadType = 'Updated-CommitMsg'
where message REGEXP 'Patch Set [0-9]+: Commit message was updated' or message REGEXP 'Uploaded patch set [0-9]+: Commit message was updated';


#Identify vote +1
update Activity
set hist_voteScore = 1
where message REGEXP 'Patch Set [0-9]+: Code-Review[+]1' 
or message REGEXP 'Patch Set [0-9]+: [^;]*[; ]*Looks good to me, but someone else must approve'
or message REGEXP 'Patch Set [0-9]+: Might be fine, but I am too lame to make sure';

#Identify vote +2
update Activity
set hist_voteScore = 2
where message REGEXP 'Patch Set [0-9]+: Code-Review[+]2' 
or message REGEXP '^Patch Set [0-9]+:[^\n]*Code-Review[ ]*[+]2'
or message REGEXP 'Patch Set [0-9]+: Looks good to me ' 
or message REGEXP 'Patch Set [0-9]+: Approved'
or message REGEXP 'Patch Set [0-9]+: Looks good to me, approved' 
or message REGEXP 'Patch Set [0-9]+: Woah! Awesome!! You rule!!!';





#Identify vote -1
update Activity
set hist_voteScore = -1
where message REGEXP 'Patch Set [0-9]+: Code-Review[-]1' 
or message REGEXP 'Patch Set [0-9]+: [-]Code-Review' 
or message REGEXP 'Patch Set [0-9]+:[^\n]*[-]Code-Review' 
or message REGEXP "Patch Set [0-9]+: [^;]*[; ]*I would prefer that you didn't merge this" 
or message REGEXP "Patch Set [0-9]+: [^;]*[; ]*I would prefer that you didn't submit this" 
or message REGEXP 'Patch Set [0-9]+: [^;]*[; ]*This need some tweaks before it is merged'
or message REGEXP 'Patch Set [0-9]+: [^;]*[; ]*I would prefer this is not merged as is'
or message REGEXP 'Patch Set [0-9]+: [^;]*[; ]*I would prefer that you didnt submit this';


#Identify vote -2
update Activity
set hist_voteScore = -2
where message REGEXP 'Patch Set [0-9]+: Code-Review[-]2' 
or message REGEXP 'Patch Set [0-9]+: [^;]*[; ]*Do not merge' 
or message REGEXP 'Patch Set [0-9]+: [^;]*[; ]*Abandoned' 
or message REGEXP 'Patch Set [0-9]+: Abandoned\n\n'
or message REGEXP '^Abandoned\n\n' 
or message REGEXP '^Abandoned$'
or message REGEXP 'Patch Set [0-9]+: [^;]*[; ]*Do not submit'
or message REGEXP 'Patch Set [0-9]+: [^;]*[; ]*This shall not be merged'; 




#Identify vote 0
update Activity
set hist_voteScore = 0
where message REGEXP 'Patch Set [0-9]+: No score';

#Identify general comments (IMPORTANT: Everytime this part is run, other type of feedback must be re-identified)
update Activity
set hist_FeedbackType = 'General'
where message REGEXP 'Patch Set [0-9]+:[^\n]*\n\n.+';

#Identify inline comments
update Activity
set hist_FeedbackType = 'Inline'
where (message REGEXP 'Patch Set [0-9]+:[^\n]*\n\n[(][0-9]+ comment[s]*[)]' or message REGEXP 'Patch Set [0-9]+:[^\n]*\n\n[(][0-9]+ inline comment[s]*[)]' or message REGEXP 'Patch Set [0-9]+: [(][0-9]+ comment[s]*[)]' or message REGEXP 'Patch Set [0-9]+: [(][0-9]+ inline comment[s]*[)]');

#Identify general and inline comments
update Activity
set hist_FeedbackType = 'General+Inline'
where  (message REGEXP 'Patch Set [0-9]+:[^\n]*\n\n[(][0-9]+ comment[s]*[)]\n\n.+' or message REGEXP 'Patch Set [0-9]+:[^\n]*\n\n[(][0-9]+ inline comment[s]*[)]\n\n.+');


#WARNING!!!# After run this sql. We must manually check the data and manually reclassify
update Activity
set hist_ignore = 1
where hist_UploadType is null and hist_voteScore is null and hist_FeedbackType is null;

#Create a backup Activity table and delete ignore rows in Activity table
CREATE TABLE TmpActivity LIKE Activity; 
INSERT TmpActivity SELECT * FROM Activity;
DELETE FROM Activity 
WHERE hist_ignore = 1 
or hist_UploadType = 'Updated-CommitMsg' 
or hist_UploadType = 'Cherry-Picked to' 
or hist_UploadType = 'Cherry-Picked from' 
or hist_UploadType = 'Rebased';


#Identify patch owner's messages. This one takes time. Should be done as the last
update Activity as hist join Reviews as ch on hist.review_id = ch.id
set hist_isPatchOwner = 1
where hist.author = ch.owner_id; 

