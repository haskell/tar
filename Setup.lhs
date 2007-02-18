> import Distribution.PackageDescription (PackageDescription(buildDepends))
> import Distribution.Setup (ConfigFlags)
> import Distribution.Simple (UserHooks(confHook),
>                             defaultMainWithHooks, defaultUserHooks, 
>                             Dependency(..), VersionRange(..))
> import Distribution.Simple.LocalBuildInfo (LocalBuildInfo)
> import qualified System.Info (os)

> main :: IO ()
> main = defaultMainWithHooks myHooks

> myHooks :: UserHooks
> myHooks = addOptionalUnixDependencyHook defaultUserHooks

> addOptionalUnixDependencyHook :: UserHooks -> UserHooks
> addOptionalUnixDependencyHook hooks = 
>    hooks { confHook = confHook hooks . addOptionalUnixDependency }

> addOptionalUnixDependency :: PackageDescription -> PackageDescription
> addOptionalUnixDependency desc = 
>   case System.Info.os of
>      "mingw32" -> desc
>      _         -> addDependency (Dependency "unix" AnyVersion) desc

> addDependency :: Dependency -> PackageDescription -> PackageDescription
> addDependency dep desc = desc { buildDepends = dep : buildDepends desc}
