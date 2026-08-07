import { PublicClientApplication } from "@azure/msal-browser";

export const msalConfig = {
    auth: {
        clientId: import.meta.env.VITE_CLIENT_ID,
        authority:
          `https://login.microsoftonline.com/${import.meta.env.VITE_TENANT_ID}`
    }
};

export const msalInstance =
    new PublicClientApplication(msalConfig);
