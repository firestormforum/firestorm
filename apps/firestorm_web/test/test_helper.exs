### EX_UNIT - TEST FRAMEWORK ###
# We probably want the test framework running
ExUnit.start()

### WALLABY - ACCEPTANCE TESTING ###
# Start wallaby for integration tests
{:ok, _} = Application.ensure_all_started(:wallaby)
# Tell wallaby where our app lives
Application.put_env(:wallaby, :base_url, FirestormWeb.Web.Endpoint.url)

### EX_MACHINA - FIXTURES ###
# Start ex_machina when we start our tests
{:ok, _} = Application.ensure_all_started(:ex_machina)

### FAKER - FAKE DATA THAT ISN'T TERRIBLE ###
Faker.start

### ECTO - STORES DEM BYTES ###
# Set our SQL sandbox mode to manual
Ecto.Adapters.SQL.Sandbox.mode(FirestormData.Repo, :manual)
