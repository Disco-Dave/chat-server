import Layout from "./Layout";
import UserRoomForm from "./UserRoomForm";

function App() {
  return (
    <Layout title="Join a Room">
      <UserRoomForm onSubmit={(form) => console.log(form)} />
    </Layout>
  );
}

export default App;
