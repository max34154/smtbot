

INSERT INTO TBOT.REQUEST (REQ_ID, USER_NAME, ACTION, PARAMETERS, SCHEDULE_NAME, STATUS, ATTEMPT, EXECUTION_RETRIES,RETRY_INTERVAL )
VALUES( 'test1', 'other-user', 'test-action2', 'test paramter 2', 'test_sch', 'N', '10', '10', '300');

INSERT INTO TBOT.REQUEST (REQ_ID, USER_NAME, ACTION, PARAMETERS, SCHEDULE_NAME, STATUS, ATTEMPT, EXECUTION_RETRIES,RETRY_INTERVAL )
VALUES( 'test2', 'max', 'test-action5', 'test paramter 2', '', 'N', '10', '10', '300');


INSERT INTO TBOT.REQUEST (REQ_ID, USER_NAME, ACTION, PARAMETERS, SCHEDULE_NAME, STATUS, ATTEMPT, EXECUTION_RETRIES,RETRY_INTERVAL )
VALUES( 'test3', 'max', 'test-action', 'test paramter 1', 'test_sch', 'W', '10', '10', '300');


INSERT INTO TBOT.REQUEST (REQ_ID, USER_NAME, ACTION, PARAMETERS, SCHEDULE_NAME, STATUS, ATTEMPT, EXECUTION_RETRIES,RETRY_INTERVAL )
VALUES( 'test4', 'max', 'test-action', 'test paramter 1', 'test_sch', 'L', '10', '10','300');


INSERT INTO TBOT.REQUEST (REQ_ID, USER_NAME, ACTION, PARAMETERS, SCHEDULE_NAME, STATUS, ATTEMPT, EXECUTION_RETRIES,RETRY_INTERVAL )
VALUES( 'test5', 'max', 'test-action', 'test paramter 1', 'test_sch', 'L', '10', '10', '300');

INSERT INTO TBOT.REQUEST (REQ_ID, USER_NAME, ACTION, PARAMETERS, SCHEDULE_NAME, STATUS, ATTEMPT, EXECUTION_RETRIES,RETRY_INTERVAL )
VALUES( 'test6', 'max', 'test-action', 'test paramter 1', 'test_sch', 'N', '10','10', '300');

INSERT INTO TBOT.REQUEST (REQ_ID, USER_NAME, ACTION, PARAMETERS, SCHEDULE_NAME, STATUS, ATTEMPT, EXECUTION_RETRIES,RETRY_INTERVAL )
VALUES( 'test7', 'max', 'test-action2', 'test paramter 2', 'test_sch', 'N', '10','10', '300');


INSERT INTO TBOT.REQUEST (REQ_ID, USER_NAME, ACTION, PARAMETERS, SCHEDULE_NAME, STATUS, ATTEMPT, EXECUTION_RETRIES,RETRY_INTERVAL )
VALUES( 'test8', 'max', 'test-action2', 'test paramter 2', 'test_sch', 'N', '10', '10','300');


INSERT INTO TBOT.REQUEST (REQ_ID, USER_NAME, ACTION, PARAMETERS, SCHEDULE_NAME, STATUS, ATTEMPT, EXECUTION_RETRIES,RETRY_INTERVAL )
VALUES( 'test9', 'other-user', 'test-action2', 'test paramter 2', 'test_sch', 'N', '10','10', '300');

INSERT INTO TBOT.REQUEST (REQ_ID, USER_NAME, ACTION, PARAMETERS, SCHEDULE_NAME, STATUS, ATTEMPT, EXECUTION_RETRIES,RETRY_INTERVAL )
VALUES( 'test10', 'max', 'test-action3', 'test paramter 2', 'test_sch', 'N', '10', '10', '300');


INSERT INTO TBOT.REQUEST (REQ_ID, USER_NAME, ACTION, PARAMETERS, SCHEDULE_NAME, STATUS, ATTEMPT, EXECUTION_RETRIES,RETRY_INTERVAL )
VALUES( 'test11', 'max', 'test-action5', 'test paramter 2', 'test_sch', 'N', '10','10', '300');


INSERT INTO TBOT.REQUEST (REQ_ID, USER_NAME, ACTION, PARAMETERS, SCHEDULE_NAME, STATUS, ATTEMPT, EXECUTION_RETRIES,RETRY_INTERVAL )
VALUES( 'test12', 'max', 'test-action5', 'test paramter 2', '', 'N', '10', '10','300');


INSERT INTO TBOT.REQUEST (REQ_ID, USER_NAME, ACTION, PARAMETERS, SCHEDULE_NAME, STATUS, ATTEMPT, EXECUTION_RETRIES,RETRY_INTERVAL )
VALUES( 'test13', 'max', 'test-action5', 'test paramter 2', '', 'N', '10', '10', '300');

INSERT INTO TBOT.REQUEST (REQ_ID, USER_NAME, ACTION, PARAMETERS, SCHEDULE_NAME, STATUS, ATTEMPT, EXECUTION_RETRIES,RETRY_INTERVAL )
VALUES( 'test14', 'max', 'test-action5', 'test paramter 2', '', 'N', '10','10', '300');

COMMIT;

INSERT INTO TBOT.ATTACHMENT (ATT_ID , ATT_REQ_ID , NAME ,CONTENT_TYPE ,BODY ) VALUES ('hex13-1', 'test13', 'test13-file1', 'image/jpg', 'body of jpgx');
INSERT INTO TBOT.ATTACHMENT (ATT_ID , ATT_REQ_ID , NAME ,CONTENT_TYPE ,BODY ) VALUES ('hex13-2', 'test13', 'test13-file2', 'image/jpg','body of jpg');
INSERT INTO TBOT.ATTACHMENT (ATT_ID , ATT_REQ_ID , NAME ,CONTENT_TYPE ,BODY ) VALUES ('hex13-3', 'test13', 'test1-file3', 'image/jpg','body of jpg');




INSERT INTO TBOT.ATTACHMENT (ATT_ID , ATT_REQ_ID , NAME ,CONTENT_TYPE ,BODY ) VALUES ('hex14-1', 'test14', 'test14-file1', 'image/jpg','body of jpg');
INSERT INTO TBOT.ATTACHMENT (ATT_ID , ATT_REQ_ID , NAME ,CONTENT_TYPE ,BODY ) VALUES ('hex14-2', 'test14', 'test14-file2', 'image/jpg','body of jpg');
INSERT INTO TBOT.ATTACHMENT (ATT_ID , ATT_REQ_ID , NAME ,CONTENT_TYPE ,BODY ) VALUES ('hex14-3', 'test14', 'test14-file3', 'image/jpg','body of jpg');

COMMIT;
