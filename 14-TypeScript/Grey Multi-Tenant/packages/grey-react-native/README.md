# @grey/react-native

React Native bindings for Grey Multi-Tenant.

## Installation

```bash
pnpm add @grey/react-native

# Optional: For secure token storage
pnpm add @react-native-async-storage/async-storage
```

## Usage

```tsx
import { GreyProvider, useAuth, useUser, useProjects } from '@grey/react-native';

function App() {
  return (
    <GreyProvider 
      baseUrl="https://api.example.com"
      storage={AsyncStorage} // Optional: for session persistence
    >
      <Navigation />
    </GreyProvider>
  );
}

function ProfileScreen() {
  const { user, isLoading } = useUser();
  const { logout } = useAuth();
  
  if (isLoading) return <ActivityIndicator />;
  if (!user) return <LoginScreen />;
  
  return (
    <View>
      <Text>Welcome, {user.name}</Text>
      <Button title="Logout" onPress={logout} />
    </View>
  );
}
```

## Features

- Session persistence with AsyncStorage
- Network state handling
- Automatic token refresh
- Secure token storage (optional)

## API

- `GreyProvider` - Context provider with storage option
- `useAuth()` - Auth state and methods
- `useUser()` - User state
- `useProjects()` - Projects CRUD
- `useNetworkStatus()` - Network connectivity
- `useSecureSession()` - Secure session management
