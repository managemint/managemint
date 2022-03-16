module ProjectConfig where

import DatabaseUtil
import Scheduler
import ScheduleFormat

import qualified Data.Map as M
import Database.Persist.MySQL hiding (get)
import Control.Monad.Trans.Reader

data ProjectConfiguration = ProjectConfiguration
    {
    }

readAndParseConfigFile :: String -> FilePath -> Entity Project -> ReaderT SqlBackend IO ProjectConfiguration
readAndParseConfigFile id path p = do
    testId <- entityKey . head <$> getPlaybooks (entityKey p) -- TODO: Remove, this is only for testing
    return $ M.fromList [(path, JobTemplate{_scheduleFormat=scheduleNext, _repoPath=path, _playbook="pb.yml", _playbookId=testId, _failCount=0, _systemJob=False, _repoIdentifier=id})]
