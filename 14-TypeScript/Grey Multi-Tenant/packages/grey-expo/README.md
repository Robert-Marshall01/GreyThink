# @grey/expo

Expo bindings for Grey Multi-Tenant.

Built on top of @grey/react-native with Expo-specific integrations.

## Installation

```bash
npx expo install @grey/expo expo-secure-store
```

## Usage

```tsx
import { GreyProvider, useAuth, useUser } from '@grey/expo';

export default function App() {
  return (
    <GreyProvider
      baseUrl="https://api.example.com"
      secureStorage // Uses expo-secure-store
    >
      <Navigation />
    </GreyProvider>
  );
}

function ProfileScreen() {
  const { user } = useUser();
  const { logout } = useAuth();
  
  return (
    <View>
      <Text>Welcome, {user?.name}</Text>
      <Button title="Logout" onPress={logout} />
    </View>
  );
}
```

## Features

- Secure token storage with expo-secure-store
- Expo Router integration
- Expo Updates compatibility
- Push notification token management

## API

Re-exports all from @grey/react-native plus:

- `GreyProvider` - Enhanced provider with secure storage
- `useSecureSession()` - Secure session management
- `usePushToken()` - Push notification token hook
