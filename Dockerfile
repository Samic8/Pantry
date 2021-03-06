FROM node:8

ARG env=dev
ENV env=${env}

# Create app directory
WORKDIR /usr/src/app

# Install app dependencies
# A wildcard is used to ensure both package.json AND package-lock.json are copied
# where available (npm@5+)
COPY package*.json ./
COPY yarn.lock ./

# If you are building your code for production
RUN yarn install --production=true

# Bundle app source
COPY . .

EXPOSE 8080

CMD ["sh", "-c", "npm run start:server:$env"]